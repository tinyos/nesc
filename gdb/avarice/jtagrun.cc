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
#include <sys/time.h>
#include <termios.h>
#include <fcntl.h>
#include <string.h>

#include "avarice.h"
#include "jtag.h"
#include "remote.h"

unsigned long getProgramCounter(void)
{
    uchar *response = NULL;
    uchar command[] = {'2', JTAG_EOM };
    unsigned long result = 0;

    response = doJtagCommand(command, sizeof(command), 4);

    if (response[3] != JTAG_R_OK)
	result = PC_INVALID;
    else
    {
	result = decodeAddress(response);

	result--; // returned value is PC + 1 as far as GDB is concerned

	// The JTAG box sees program memory as 16-bit wide locations. GDB
	// sees bytes. As such, double the PC value.
	result *= 2;
    }

    delete [] response;
    return result;
}

bool setProgramCounter(unsigned long pc)
{
    uchar *response = NULL;
    uchar command[] = {'3', 0, 0, 0, JTAG_EOM };
    bool result;

    // See decoding in getProgramCounter
    encodeAddress(&command[1], pc / 2 + 1);

    response = doJtagCommand(command, sizeof(command), 1);

    result = response[0] == JTAG_R_OK;

    delete [] response;

    return result;
}

bool resetProgram(void)
{
    return doSimpleJtagCommand('x', 1);
}

bool interruptProgram(void)
{
    // Just ignore the returned PC. It appears to be wrong if the most
    // recent instruction was a branch.
    return doSimpleJtagCommand('F', 4);
}

bool resumeProgram(void)
{
    return doSimpleJtagCommand('G', 0);
}

bool jtagSingleStep(void)
{
    return doSimpleJtagCommand('1', 1);
}

bool jtagContinue(bool setCodeBreakpoints)
{
    updateBreakpoints(setCodeBreakpoints); // download new bp configuration

    if (!doSimpleJtagCommand('G', 0))
    {
	gdbOut("Failed to continue\n");
	return true;
    }

    for (;;)
    {
	int maxfd;
	fd_set readfds;
	bool breakpoint = false, gdbInterrupt = false;

	// Now that we are "going", wait for either a response from the JTAG
	// box or a nudge from GDB.
	debugOut("Waiting for input.\n");

	// Check for input from JTAG ICE (breakpoint, sleep, info, power)
	// or gdb (user break)
	FD_ZERO (&readfds);
	FD_SET (gdbFileDescriptor, &readfds);
	FD_SET (jtagBox, &readfds);
	maxfd = jtagBox > gdbFileDescriptor ? jtagBox : gdbFileDescriptor;

	int numfds = select(maxfd + 1, &readfds, 0, 0, 0);
	unixCheck(numfds, "GDB/JTAG ICE communications failure");

	if (FD_ISSET(gdbFileDescriptor, &readfds))
	{
	    int c = getDebugChar();
	    if (c == 3) // interrupt
	    {
		debugOut("interrupted by GDB\n");
		gdbInterrupt = true;
	    }
	    else
		debugOut("Unexpected GDB input `%02x'\n", c);
	}

	// Read all extant responses (there's a small chance there could
	// be more than one)

	// Note: In case of a gdb interrupt, it's possible that we will
	// receive one of these responses before interruptProgram is 
	// called. This should not cause a problem as the normal retry
	// mechanism should eat up this response.
	// It might be cleaner to handle JTAG_R_xxx in sendJtagCommand

	// Check the JTAG ICE's message. It can indicate a breakpoint
	// which causes us to return, a sleep status change (ignored),
	// power "event" -- whatever that is (ignored), or a byte of
	// info sent by the program (currently ignored, could be used
	// for something...)
	uchar response;
	while (read(jtagBox, &response, 1) == 1)
	{
	    uchar buf[2];
	    int count;

	    debugOut("JTAG box sent %c\n", response);
	    switch (response)
	    {
	    case JTAG_R_BREAK:
		count = timeout_read(jtagBox, buf, 2, JTAG_RESPONSE_TIMEOUT);
		jtagCheck(count);
		check(count == 2, JTAG_CAUSE);
		breakpoint = true;
		break;
	    case JTAG_R_INFO: case JTAG_R_SLEEP:
		// we could do something here, esp. for info
		count = timeout_read(jtagBox, buf, 2, JTAG_RESPONSE_TIMEOUT);
		jtagCheck(count);
		check(count == 2, JTAG_CAUSE);
		break;
	    case JTAG_R_POWER:
		// apparently no args?
		break;
	    default:
		debugOut("Unknown response\n");
		break; 
	    }
	}

	// We give priority to user interrupts
	if (gdbInterrupt)
	    return false;
	if (breakpoint)
	    return true;
    }
}

