/*
 *	avarice - The "avarice" program.
 *	Copyright (C) 2001 Scott Finneran
 *      Copyright (C) 2002 Intel Corporation
 *
 *	This program is free software; you can redistribute it and/or modify
 *	it under the terms of the GNU General Public License Version 2
 *      as published by the Free Software Foundation.
 *
 *	This program is distributed in the hope that it will be useful,
 *	but WITHOUT ANY WARRANTY; without even the implied warranty of
 *	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *	GNU General Public License for more details.
 *
 *	You should have received a copy of the GNU General Public License
 *	along with this program; if not, write to the Free Software
 *	Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111, USA.
 *
 * This file contains functions for interfacing with the JTAG box.
 */


#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/stat.h>
#include <termios.h>
#include <fcntl.h>
#include <string.h>

#include "avarice.h"
#include "jtag.h"

// This flag is used to select the address space values for the
// jtagRead() and jtagWrite() commands.
bool programmingEnabled = false;

void enableProgramming(void)
{
    programmingEnabled = true;
    check(doSimpleJtagCommand(0xa3, 1), 
	      "JTAG ICE: Failed to enable programming");
}


void disableProgramming(void)
{
    programmingEnabled = false;
    check(doSimpleJtagCommand(0xa4, 1), 
	  "JTAG ICE: Failed to disable programming");
}


void eraseProgramMemory(void)
{
    check(doSimpleJtagCommand(0xa5, 1), 
	  "JTAG ICE: Failed to erase program memory");
}

void eraseProgramPage(unsigned long address)
{
    uchar *response = NULL;
    uchar command[] = { 0xa1, 0, 0, 0, JTAG_EOM };

    command[1] = address >> 8;
    command[2] = address;

    response = doJtagCommand(command, sizeof(command), 1);
    check(response[0] == JTAG_R_OK, "Page erase failed\n");

    delete [] response;
}

void downloadToTarget(const char* filename)
{
    // Basically, we just open the file and copy blocks over to the JTAG
    // box.
    struct stat ifstat;
    bool partiallyFilledPage = false;

    unixCheck(stat(filename, &ifstat), "Can't stat() file %s", filename);

    // Let's see how big it is.
    int remaining = ifstat.st_size;

    int chunk = 256; // maximum chunk size (equiv to JTAG buffer size).

    uchar *buffer = new uchar[chunk];

    // Open the input file.
    int inputFile = open(filename, O_RDONLY);
    unixCheck(inputFile, "Could not open input file %s", filename);

    // Configure for JTAG download/programming
    // XXX: These are device dependent
    setJtagParameter(0x88, 0x00);
    setJtagParameter(0x89, 0x01);
    setJtagParameter(0x8A, 0x08);

    // First erase the flash
    enableProgramming();
    eraseProgramMemory();
    disableProgramming();

    // And then send down the new image.

    statusOut("Downloading to target.");
    statusFlush();

    enableProgramming();

    int address = 0;
    while (remaining > 0)
    {
	int count = remaining > chunk ? chunk : remaining;

	// In
	check(read(inputFile, buffer, count) == count,
	      "Error reading input file %s", filename);

	// Out
	check(jtagWrite(address, count, buffer), "Error writing to target");

	// Show progress.
	statusOut(".");
	statusFlush();

	if (count < chunk)
	{
	    // And a write to last address in page triggers programming.
	    uchar confirmData[] = {0xff, 0xff};

	    // determine last word in page.
	    int lastPageAddress = address | (chunk - sizeof confirmData);

	    check(jtagWrite(lastPageAddress, sizeof(confirmData), confirmData),
		  "Error writing to target\n");
	}

 	address += chunk;
	remaining -= chunk;
    }

    disableProgramming();

    unixCheck(close(inputFile), "Error closing %s", filename);

    statusOut("\nDownload complete.\n");

    delete [] buffer;
}
