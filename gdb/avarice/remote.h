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
 * Interface definition for the include/remote.c file.
 */

#ifndef INCLUDE_REMOTE_H
#define INCLUDE_REMOTE_H

/** File descriptor for gdb communication. -1 before connection. **/
extern int gdbFileDescriptor;

/** Talk to gdb over file descriptor 'fd' **/
void setGdbFile(int fd);

#define GDB_CAUSE "gdb communications failure"

/** If status < 0: Report gdb communication error & exit **/
void gdbCheck(int status);

/** Return single char read from gdb. Abort in case of problem,
    exit cleanly if EOF detected on gdbFileDescriptor. **/
int getDebugChar(void);

/** printf 'fmt, ...' to gdb **/
void gdbOut(const char *fmt, ...);
void vgdbOut(const char *fmt, va_list args);

/** GDB remote protocol interpreter */
void talkToGdb(void);

#endif /* INCLUDE_REMOTE_H */
