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
 */

#include <stdio.h>
#include <stdarg.h>
#include <errno.h>
#include <stdlib.h>
#include <string.h>

#include "avarice.h"
#include "remote.h"

bool debugMode = false;

void vdebugOut(const char *fmt, va_list args)
{
    if (debugMode)
    {
	(void)vprintf(fmt, args);
    }
}

void debugOut(const char *fmt, ...)
{
    va_list args;
    va_start(args, fmt);
    vdebugOut(fmt, args);
    va_end(args);
}

void vstatusOut(const char *fmt, va_list args)
{
    vprintf(fmt, args);
}

void statusOut(const char *fmt, ...)
{
    va_list args;
    va_start(args, fmt);
    vstatusOut(fmt, args);
    va_end(args);
}

void statusFlush()
{
    fflush(stdout);
}

static void check_1(bool printUnixError, const char *fmt, va_list args)
{
    int en = errno;
#ifdef va_copy
    va_list copy_args;

    va_copy(copy_args, args);
    vstatusOut(fmt, copy_args);
#else
    vstatusOut(fmt, args);
#endif
    if (printUnixError)
	statusOut(": %s", strerror(en));
    statusOut("\n");

    vgdbOut(fmt, args);
    if (printUnixError)
	gdbOut(": %s", strerror(en));
    gdbOut("\n");

    exit(EXIT_FAILURE);
}

void check(bool ok, const char *fmt, ...)
{
    va_list args;
    va_start(args, fmt);
    if (!ok)
	check_1(false, fmt, args);
    va_end(args);
}

void unixCheck(int status, const char *fmt, ...)
{
    va_list args;
    va_start(args, fmt);
    if (status < 0)
	check_1(true, fmt, args);
    va_end(args);
}

