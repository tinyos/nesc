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

/* There are apparently a total of three hardware breakpoints 
   (the docs claim four, but documents 2 breakpoints accessible via 
   non-existent parameters). 

   In summary, there is one code-only breakpoint, and 2 breakpoints which
   can be:
   - 2 code breakpoints
   - 2 1-byte data breakpoints
   - 1 code and 1-byte data breakpoint
   - 1 ranged code breakpoint
   - 1 ranged data breakpoint

   Currently we're ignoring ranges, so we allow up to 3 breakpoints,
   of which a maximum of 2 are data breakpoints. This is easily handled
   by keeping each kind of breakpoint separate.

   In the future, we could support "software" breakpoints too (though it
   seems mildly painful)

   See avrIceProtocol.txt for full details.
*/

enum {
  // We distinguish the total possible breakpoints and those for each type
  // (code or data) - see above
  MAX_BREAKPOINTS_CODE = 4,
  MAX_BREAKPOINTS_DATA = 2,
  MAX_BREAKPOINTS = 4
};

struct breakpoint
{
    unsigned int address;
    bpType type;
};

static breakpoint bpCode[MAX_BREAKPOINTS_CODE], bpData[MAX_BREAKPOINTS_DATA];
static int numBreakpointsCode, numBreakpointsData;

bool codeBreakpointAt(unsigned int address) 
{
  address /= 2;
  for (int i = 0; i < numBreakpointsCode; i++)
    if (bpCode[i].address == address)
      return true;
  return false;
}

bool codeBreakpointBetween(unsigned int start, unsigned int end) 
{
  start /= 2; end /= 2;
  for (int i = 0; i < numBreakpointsCode; i++)
    if (bpCode[i].address >= start && bpCode[i].address < end)
      return true;
  return false;
}

void deleteAllBreakpoints(void)
{
    numBreakpointsData = numBreakpointsCode = 0;
}

bool stopAt(unsigned int address)
{
    uchar zero = 0;
    jtagWrite(BREAKPOINT_SPACE_ADDR_OFFSET + address / 2, 1, &zero);
}

bool addBreakpoint(unsigned int address, bpType type, unsigned int length)
{
    breakpoint *bp;

    debugOut("BP ADD type: %d  addr: 0x%x ", type, address);

    // Respect overall breakpoint limit
    if (numBreakpointsCode + numBreakpointsData == MAX_BREAKPOINTS)
      return false;

    // There's a spare breakpoint, is there one of the appropriate type 
    // available?
    if (type == CODE)
    {
	if (numBreakpointsCode == MAX_BREAKPOINTS_CODE)
	{
	    debugOut("FAILED\n");
	    return false;
	}

	bp = &bpCode[numBreakpointsCode++];

	// The JTAG box sees program memory as 16-bit wide locations. GDB
	// sees bytes. As such, halve the breakpoint address.
	address /= 2;
    }
    else // data breakpoint
    {
	if (numBreakpointsData == MAX_BREAKPOINTS_DATA)
	{
	    debugOut("FAILED\n");
	    return false;
	}

	bp = &bpData[numBreakpointsData++];
    }

    bp->address = address;
    bp->type = type;

    debugOut(" ADDED\n");
    return true;
}


bool deleteBreakpoint(unsigned int address, bpType type, unsigned int length)
{
    breakpoint *bp;
    int *numBp;

    debugOut("BP DEL type: %d  addr: 0x%x ", type, address);

    if (type == CODE)
    {
	bp = bpCode;
	numBp = &numBreakpointsCode;
	// The JTAG box sees program memory as 16-bit wide locations. GDB
	// sees bytes. As such, halve the breakpoint address.
	address /= 2;
    }
    else // data
    {
	bp = bpData;
	numBp = &numBreakpointsData;
    }

    // Find and squash the removed breakpoint
    for (int i = 0; i < *numBp; i++)
    {
	if (bp[i].type == type && bp[i].address == address)
	{
	    debugOut("REMOVED %d\n", i);
	    (*numBp)--;
	    memmove(&bp[i], &bp[i + 1], (*numBp - i) * sizeof(breakpoint));
	    return true;
	}
    }
    debugOut("FAILED\n");
    return false;
}


void updateBreakpoints(bool setCodeBreakpoints)
{
    unsigned char bpMode = 0x00;
    int bpC = 0, bpD = 0;
    breakpoint *bp;

    debugOut("updateBreakpoints\n");
    
    if (!setCodeBreakpoints)
	bpC = numBreakpointsCode;

    // BP 0 (aka breakpoint Z0).
    // Send breakpoint array down to the target.
    // BP 1 is activated by writing a 1 to BP address space.
    // note: BP1 only supports code space breakpoints.
    if (bpC < numBreakpointsCode)
    {
	uchar zero = 0;
	jtagWrite(BREAKPOINT_SPACE_ADDR_OFFSET + bpCode[bpC++].address, 1, &zero);
    }

    // BP 1 (aka breakpoint Z1).
    // Send breakpoint array down to the target.
    // BP 1 is activated by writing a 1 to BP address space.
    // note: BP1 only supports code space breakpoints.
    if (bpC < numBreakpointsCode)
    {
	uchar one = 1;
	jtagWrite(BREAKPOINT_SPACE_ADDR_OFFSET + bpCode[bpC++].address, 1, &one);
    }

    // Set X & Y breakpoints. Code is cut&pasty, but it's only two copies and
    // has tons of parameters.

    // Find next breakpoint
    bp = NULL;
    if (bpC < numBreakpointsCode)
	bp = &bpCode[bpC++];
    else if (bpD < numBreakpointsData)
	bp = &bpData[bpD++];

    // BP 2 (aka breakpoint X).
    if (bp)
    {
	setJtagParameter(JTAG_P_BP_X_HIGH, bp->address >> 8);
	setJtagParameter(JTAG_P_BP_X_LOW, bp->address & 0xff);

	bpMode |= 0x20; // turn on this breakpoint
	switch (bp->type)
	{
	case READ_DATA:
	    bpMode |= 0x00;
	    break;
	case WRITE_DATA:
	    bpMode |= 0x04;
	    break;
	case ACCESS_DATA:
	    bpMode |= 0x08;
	    break;
	case CODE:
	    bpMode |= 0x0c;
	    break;
        case NONE:
        default:
            break;
	}


	// Find next breakpoint
	bp = NULL;
	if (bpC < numBreakpointsCode)
	    bp = &bpCode[bpC++];
	else if (bpD < numBreakpointsData)
	    bp = &bpData[bpD++];

	// BP 3 (aka breakpoint Y).
	if (bp)
	{
	    setJtagParameter(JTAG_P_BP_Y_HIGH, bp->address >> 8);
	    setJtagParameter(JTAG_P_BP_Y_LOW, bp->address & 0xff);

	    // Only used when BP2 is on, so this is correcy
	    bpMode |= 0x10; // turn on this breakpoint
	    switch (bp->type)
	    {
	    case READ_DATA:
		bpMode |= 0x00;
		break;
	    case WRITE_DATA:
		bpMode |= 0x01;
		break;
	    case ACCESS_DATA:
		bpMode |= 0x02;
		break;
	    case CODE:
		bpMode |= 0x03;
		break;
	    case NONE:
	    default:
		break;
	    }
	}

	setJtagParameter(JTAG_P_BP_MODE, bpMode);
    }
}
