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

void setJtagParameter(uchar item, uchar newValue)
{
    uchar *response = NULL;
    uchar command[] = {'B', 0, 0, JTAG_EOM };

    command[1] = item;
    command[2] = newValue;

    response = doJtagCommand(command, sizeof(command), 1);
    check(response[0] == JTAG_R_OK, "JTAG ICE: Unknown parameter\n");

    delete [] response;
}

uchar getJtagParameter(uchar item)
{
    uchar *response = NULL;
    uchar command[] = {'q', 0, JTAG_EOM };
    unsigned char result = 0;

    command[1] = item;
    response = doJtagCommand(command, sizeof(command), 2);
    check(response[1] == JTAG_R_OK, "JTAG ICE: Unknown parameter\n");

    result = response[0];

    delete [] response;

    return result;
}


