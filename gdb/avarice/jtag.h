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
 * This file contains functions for interfacing with the GDB remote protocol.
 */

#ifndef JTAG_H
#define JTAG_H

// various enums
enum
{
    // Address space selector values
    ADDR_PROG_SPACE_PROG_ENABLED      = 0xb0,
    ADDR_PROG_SPACE_PROG_DISABLED     = 0xa0,
    ADDR_DATA_SPACE                   = 0x20,
    ADDR_EEPROM_SPACE                 = 0x22,
    ADDR_FUSE_SPACE                   = 0xB2,
    ADDR_LOCK_SPACE                   = 0xB3,
    ADDR_SIG_SPACE                    = 0xB4,
    ADDR_BREAKPOINT_SPACE             = 0x60,

    // Address space offsets
    DATA_SPACE_ADDR_OFFSET            = 0x800000,

    EEPROM_SPACE_ADDR_OFFSET          = 0x810000,

    FUSE_SPACE_ADDR_OFFSET            = 0x820000,

    LOCK_SPACE_ADDR_OFFSET            = 0x830000,

    SIG_SPACE_ADDR_OFFSET             = 0x840000,

    BREAKPOINT_SPACE_ADDR_OFFSET      = 0x900000,

    ADDR_SPACE_MASK = (DATA_SPACE_ADDR_OFFSET   |
                       EEPROM_SPACE_ADDR_OFFSET |
                       FUSE_SPACE_ADDR_OFFSET   |
                       LOCK_SPACE_ADDR_OFFSET   |
                       SIG_SPACE_ADDR_OFFSET    |
                       BREAKPOINT_SPACE_ADDR_OFFSET),

    // Lock Bit Values
    LOCK_BITS_ALL_UNLOCKED            = 0xff,

    // Fuse Bit Values
    FUSE_M103C                        = 0x02,
    FUSE_WDTON                        = 0x01,

    FUSE_OCDEN                        = 0x80,
    FUSE_JTAGEN                       = 0x40,
    FUSE_SPIEN                        = 0x20,
    FUSE_CKOPT                        = 0x10,
    FUSE_EESAVE                       = 0x08,
    FUSE_BOOTSZ1                      = 0x04,
    FUSE_BOOTSZ0                      = 0x02,
    FUSE_BOOTRST                      = 0x01,

    FUSE_BODLEVEL                     = 0x80,
    FUSE_BODEN                        = 0x40,
    FUSE_SUT1                         = 0x20,
    FUSE_SUT0                         = 0x10,
    FUSE_CKSEL3                       = 0x08,
    FUSE_CKSEL2                       = 0x04,
    FUSE_CKSEL1                       = 0x02,
    FUSE_CKSEL0                       = 0x01,

    // Comms link bit rates
    BIT_RATE_9600                     = 0xf4,
    BIT_RATE_14400                    = 0xf8,
    BIT_RATE_19200                    = 0xfa,
    BIT_RATE_38400                    = 0xfd,
    BIT_RATE_57600                    = 0xfe,
    BIT_RATE_115200                   = 0xff,

    // Breakpoints (match values returned by JTAG box).
    BREAKPOINT_NONE                   = 0x00,
    BREAKPOINT_X                      = 0x04,
    BREAKPOINT_Y                      = 0x08,
    BREAKPOINT_Z                      = 0x10,


    // Responses from JTAG ICE
    JTAG_R_OK			      = 'A',
    JTAG_R_BREAK		      = 'B',
    JTAG_R_INFO			      = 'G',
    JTAG_R_FAILED		      = 'F',
    JTAG_R_SYNC_ERROR		      = 'E',
    JTAG_R_SLEEP		      = 'H',
    JTAG_R_POWER		      = 'I',

    // JTAG parameters
    JTAG_P_BITRATE		      = 'b',
    JTAG_P_SW_VERSION		      = 0x7b,
    JTAG_P_HW_VERSION		      = 0x7a,
    JTAG_P_CLOCK		      = 0x86,
    JTAG_P_TIMERS_RUNNING	      = 0xa0,
    JTAG_P_BP_FLOW		      = 0xa1,
    JTAG_P_BP_X_HIGH		      = 0xa2,
    JTAG_P_BP_X_LOW		      = 0xa3,
    JTAG_P_BP_Y_HIGH		      = 0xa4,
    JTAG_P_BP_Y_LOW		      = 0xa5,
    JTAG_P_BP_MODE		      = 0xa6,

    // JTAG commands
    JTAG_C_SET_DEVICE_DESCRIPTOR      = 0xA0,

    MAX_JTAG_COMM_ATTEMPS	      = 10,
    MAX_JTAG_SYNC_ATTEMPS	      = 3,

    // JTAG communication timeouts, in microseconds
    // RESPONSE is for the first response byte
    // COMM is for subsequent response bytes
    JTAG_RESPONSE_TIMEOUT	      = 500000,
    JTAG_COMM_TIMEOUT		      = 100000
};

enum {
    PC_INVALID			      = 0xffffffff
};

// The Sync_CRC/EOP message terminator (no real CRC in sight...)
#define JTAG_EOM 0x20, 0x20

// A generic error message when nothing good comes to mind
#define JTAG_CAUSE "JTAG ICE communication failed"

enum deviceType {
    DEV_ATMEGA_16,
    DEV_ATMEGA_162,
    DEV_ATMEGA_169,
    DEV_ATMEGA_323,
    DEV_ATMEGA_32,
    DEV_ATMEGA_128
};

// The file descriptor used while talking to the JTAG ICE
extern int jtagBox;

// Whether we are in "programming mode" (changes how program memory
// is written, apparently)
extern bool programmingEnabled;

// Basic JTAG I/O
// -------------

/** If status < 0: Report JTAG ICE communication error & exit **/
void jtagCheck(int status);

/** Initialise the serial port specified by jtagDeviceName for JTAG ICE access
 **/
void initJtagPort(char *jtagDeviceName);

/** A timed-out read from file descriptor 'fd'.

    'timeout' is in microseconds, it is the maximum interval within which
    the read must make progress (i.e., it's a per-byte timeout)

    Returns the number of bytes read or -1 for errors other than timeout.

    Note: EOF and timeout cannot be distinguished
**/
int timeout_read(int fd, void *buf, size_t count, unsigned long timeout);

/** Decode 3-byte big-endian address **/
unsigned long decodeAddress(uchar *buffer);

/** Encode 3-byte big-endian address **/
void encodeAddress(uchar *buffer, unsigned long x);

/** Send a command to the jtag, with retries, and return the 'responseSize' 
    byte response. Aborts avarice in case of to many failed retries.

    Returns a dynamically allocated buffer containing the reponse (caller
    must free)
**/
uchar *doJtagCommand(uchar *command, int  commandSize, int responseSize);

/** Simplified form of doJtagCommand:
    Send 1-byte command 'cmd' to JTAG ICE, with retries, expecting a 
    'responseSize' byte reponse.

    Return true if responseSize is 0 or if last response byte is
    JTAG_R_OK
**/
bool doSimpleJtagCommand(uchar cmd, int responseSize);

/** Send initial configuration to setup the JTAG box itself. 
    If attach is true, the currently running program is attached
    (Note: when attaching, fuse bits cannot be set so debugging must
    have been enabled earlier)
 **/
void initJtagBox(bool attach);


// Breakpoints
// -----------

enum bpType
{
    NONE,           // disabled.
    CODE,           // normal code space breakpoint.
    WRITE_DATA,     // write data space breakpoint (ie "watch").
    READ_DATA,      // read data space breakpoint (ie "watch").
    ACCESS_DATA,    // read/write data space breakpoint (ie "watch").
};

/** Clear out the breakpoints. */
void deleteAllBreakpoints(void);

/** Delete breakpoint at the specified address. */
bool deleteBreakpoint(unsigned int address, bpType type, unsigned int length);

/** Add a code breakpoint at the specified address. */
bool addBreakpoint(unsigned int address, bpType type, unsigned int length);

/** Send the breakpoint details down to the JTAG box. */
void updateBreakpoints(bool setCodeBreakpoints);

/** True if there is a breakpoint at address */
bool codeBreakpointAt(unsigned int address);

/** True if there is a breakpoint between start (inclusive) and 
    end (exclusive) */
bool codeBreakpointBetween(unsigned int start, unsigned int end);

bool stopAt(unsigned int address);

// Miscellaneous
// -------------

/** Set JTAG ICE parameter 'item' to 'newValue' **/
void setJtagParameter(uchar item, uchar newValue);

/** Return value of JTAG ICE parameter 'item' **/
uchar getJtagParameter(uchar item);

// Writing to program memory
// -------------------------

/** Switch to faster programming mode, allows chip erase */
void enableProgramming(void);

/** Switch back to normal programming mode **/
void disableProgramming(void);

/** Erase all chip memory **/
void eraseProgramMemory(void);

/** Download an image contained in the specified file. */
void downloadToTarget(const char* filename);

// Running, single stepping, etc
// -----------------------------

/** Retrieve the current Program Counter value, or PC_INVALID if fails */
unsigned long getProgramCounter(void);

/** Set program counter to 'pc'. Return true iff successful **/
bool setProgramCounter(unsigned long pc);

/** Reset AVR. Return true iff successful **/
bool resetProgram(void);

/** Interrupt AVR. Return true iff successful **/
bool interruptProgram(void);

/** Resume program execution. Return true iff successful.
    Note: the gdb 'continue' command is handled by jtagContinue,
    this is just the low level command to resume after interruptProgram
**/
bool resumeProgram(void);

/** Issue a "single step" command to the JTAG box. 
    Return true iff successful **/
bool jtagSingleStep(void);

/** Send the program on it's merry way, and wait for a breakpoint or
    input from gdb.
    Return true for a breakpoint, false for gdb input. **/
bool jtagContinue(bool setCodeBreakpoints);

// R/W memory
// ----------

/** Read 'numBytes' from target memory address 'addr'.

    The memory space is selected by the high order bits of 'addr' (see
    above).

    Returns a dynamically allocated buffer with the requested bytes if
    successful (caller must free), or NULL if the read failed.
 **/
uchar *jtagRead(unsigned long addr, unsigned int numBytes);

/** Write 'numBytes' bytes from 'buffer' to target memory address 'addr'

    The memory space is selected by the high order bits of 'addr' (see
    above).

    Returns true for a successful write, false for a failed one.

    Note: The current behaviour for program-space writes is pretty
    weird (does not match jtagRead). See comments in jtagrw.cc.
**/
bool jtagWrite(unsigned long addr, unsigned int numBytes, uchar buffer[]);

#endif
