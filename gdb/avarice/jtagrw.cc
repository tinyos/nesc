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
#include <assert.h>

#include "avarice.h"
#include "jtag.h"
#include "remote.h"


/** Return the memory space code for the memory space indicated by the
    high-order bits of '*addr'. Also clear these high order bits in '*addr'
**/
static uchar memorySpace(unsigned long *addr)
{
    int mask;

    // We can't just mask the bits off, because 0x10000->0x1ffff are
    // valid code addresses
    if (*addr & 0x800000)
    {
	mask = *addr & ADDR_SPACE_MASK;
	*addr &= ~ADDR_SPACE_MASK;
    }
    else
	mask = 0;

    switch (mask)
    {
    case  EEPROM_SPACE_ADDR_OFFSET: return ADDR_EEPROM_SPACE;
    case FUSE_SPACE_ADDR_OFFSET: return ADDR_FUSE_SPACE;
    case LOCK_SPACE_ADDR_OFFSET: return ADDR_LOCK_SPACE;
    case SIG_SPACE_ADDR_OFFSET: return ADDR_SIG_SPACE;
    case BREAKPOINT_SPACE_ADDR_OFFSET: return ADDR_BREAKPOINT_SPACE;
    case DATA_SPACE_ADDR_OFFSET: return ADDR_DATA_SPACE;
    default: return 0; // program memory, handled specially
    }
}

static void swapBytes(uchar *buffer, int count)
{
    assert(!(count & 1));
    for (unsigned int i = 0; i < count; i += 2)
    {
	uchar temp = buffer[i];
	buffer[i] = buffer[i + 1];
	buffer[i + 1] = temp;
    }
}


uchar *jtagRead(unsigned long addr, unsigned int numBytes)
{
    uchar *response;
    int whichSpace = 0;
    uchar command[] = { 'R', 0, 0, 0, 0, 0, JTAG_EOM }; 

    if (numBytes == 0)
    {
	response = new uchar[1];
	response[0] = '\0';
	return response;
    }

    debugOut("jtagRead ");
    whichSpace = memorySpace(&addr);
    if (whichSpace)
    {
	command[1] = whichSpace;
	if (numBytes > 256)
	    return NULL;
	command[2] = numBytes - 1;
	encodeAddress(&command[3], addr);

	// Response will be the number of data bytes with an 'A' at the
	// start and end. As such, the response size will be number of bytes
	// + 2. Then add an additional byte for the trailing zero (see
	// protocol document).

	response = doJtagCommand(command, sizeof command, numBytes + 2);

	if (response[numBytes + 1] == JTAG_R_OK)
	    return response;

	delete [] response;

	return NULL;
    }
    else
    {

	// Reading program memory
	whichSpace = programmingEnabled ?
	    ADDR_PROG_SPACE_PROG_ENABLED : ADDR_PROG_SPACE_PROG_DISABLED;

	// Program space is 16 bits wide, with word reads
	int numLocations;
	if (addr & 1)
	    numLocations = (numBytes + 2) / 2;
	else
	    numLocations = (numBytes + 1) / 2;
	if (numLocations > 256)
	    return false;

	command[1] = whichSpace;
	command[2] = numLocations - 1;
	encodeAddress(&command[3], addr / 2);

	response = doJtagCommand(command, sizeof command, numLocations * 2 + 2);

	if (response[numLocations * 2 + 1] == JTAG_R_OK)
	{
	    // Programming mode and regular mode are byte-swapped...
	    if (!programmingEnabled)
		swapBytes(response, numLocations * 2);

	    if (addr & 1)
		// we read one byte early. move stuff down.
		memmove(response, response + 1, numBytes);

	    return response;
	}
    }

    delete [] response;
    
    return NULL;
}

bool jtagWrite(unsigned long addr, unsigned int numBytes, uchar buffer[])
{
    uchar *response;
    int whichSpace = 0;
    unsigned int numLocations;
    uchar command[] = { 'W', 0, 0, 0, 0, 0, JTAG_EOM }; 

    if (numBytes == 0)
	return true;

    debugOut("jtagWrite ");
    whichSpace = memorySpace(&addr);

    if (whichSpace)
	numLocations = numBytes;
    else
    {
	// Writing program memory, which is word (16-bit) addressed

	// We don't handle odd lengths or start addresses
	if ((numBytes & 1) || (addr & 1))
	    return false;

	addr /= 2;
	numLocations = numBytes / 2;

	if (programmingEnabled)
	    whichSpace = ADDR_PROG_SPACE_PROG_ENABLED;
	else
	{
	    whichSpace = ADDR_PROG_SPACE_PROG_DISABLED;
	    swapBytes(buffer, numBytes);
#if 0
	    // Doesn't work out of programming mode, as far as I can tell
	    // (there's probably some magic that would make it work, but...)
	    return false;
#endif
	}
    }

    // This is the maximum write size
    if (numLocations > 256)
	return false;

    // Writing is a two part process

    // Part 1: send the address
    command[1] = whichSpace;
    command[2] = numLocations - 1;
    encodeAddress(&command[3], addr);

    response = doJtagCommand(command, sizeof command, 0);
    if (!response)
	return false;
    delete [] response;

    // Part 2: send the data in the following form:
    // h [data byte]...[data byte] __

    // Before we begin, a little note on the endianness.
    // Firstly, data space is 8 bits wide and the only data written by
    // this routine will be byte-wide, so endianness does not matter.

    // For code space, the data is 16 bit wide. The data appears to be
    // formatted big endian in the processor memory. The JTAG box
    // expects little endian data. The object files output from GCC are
    // already word-wise little endian.

    // As this data is already formatted, and currently the only writes
    // to program space are for code download, it is simpler at this
    // stage to simply pass the data straight through. This may need to
    // change in the future.
    uchar *txBuffer = new uchar[numBytes + 3]; // allow for header and trailer
    txBuffer[0] = 'h';

    memcpy(&txBuffer[1], buffer, numBytes);

    txBuffer[numBytes + 1] = ' ';
    txBuffer[numBytes + 2] = ' ';

    response = doJtagCommand(txBuffer, numBytes + 3, 1);
    delete [] txBuffer;

    if (!response)
	return false;
    delete [] response;

    return true;
}

