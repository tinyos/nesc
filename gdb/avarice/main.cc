/*
 *	avarice - The "avarice" program.
 *	Copyright (C) 2001 Scott Finneran
 *      Copyright (C) 2002 Intel Corporation
 *
 *	This program is free software; you can redistribute it and/or modify
 *	it under the terms of the GNU General Public License Version 2
 *      as published by the Free Software Foundation.
 *
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
 * This file contains the main() & support for avrjtagd.
 */


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <termios.h>
#include <arpa/inet.h>
#include <netdb.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <fcntl.h>
#include <sys/types.h>
#include <unistd.h>

#include "avarice.h"
#include "remote.h"
#include "jtag.h"

bool ignoreInterrupts;

static int makeSocket(struct sockaddr_in *name, unsigned short int port)
{
    int sock;
    int tmp;
    struct protoent *protoent;

    sock = socket(PF_INET, SOCK_STREAM, 0);
    gdbCheck(sock);

    // Allow rapid reuse of this port.
    tmp = 1;
    gdbCheck(setsockopt(sock, SOL_SOCKET, SO_REUSEADDR, (char *)&tmp, sizeof(tmp)));

    // Enable TCP keep alive process.
    tmp = 1;
    gdbCheck(setsockopt(sock, SOL_SOCKET, SO_KEEPALIVE, (char *)&tmp, sizeof(tmp)));

    gdbCheck(bind(sock, (struct sockaddr *)name, sizeof(*name)));

    protoent = getprotobyname("tcp");
    check(protoent != NULL, "tcp protocol unknown (oops?)");

    tmp = 1;
    gdbCheck(setsockopt(sock, protoent->p_proto, TCP_NODELAY,
			(char *)&tmp, sizeof(tmp)));

    return sock;
}


static void initSocketAddress(struct sockaddr_in *name,
			      const char *hostname, unsigned short int port)
{
    struct hostent *hostInfo;

    name->sin_family = AF_INET;
    name->sin_port = htons(port);
    hostInfo = gethostbyname(hostname);
    check(hostInfo != NULL, "Unknown host %s", hostname);
    name->sin_addr = *(struct in_addr *)hostInfo->h_addr;
}


static void usage(const char *progname)
{
    fprintf(stderr,
	    "Usage: %s [OPTION]... <host-name> <port>\n\n",
	    progname);
    fprintf(stderr, "Options:\n");
    fprintf(stderr,
	    "  --help                      Print this message.\n\n");
    fprintf(stderr,
	    "  -d, --debug                 Enable printing of debug "
	    "information.\n\n");
    fprintf(stderr,
	    "  --detach                    Detach once synced with JTAG ICE\n\n");
    fprintf(stderr,
	    "  --capture                   Capture running program."
	    "     Note: debugging must have been enabled prior to starting the program.\n"
	    "           (e.g., by running avarice earlier)\n\n");
    fprintf(stderr,
	    "  --ignore-intr               Automatically step over interrupts."
	    "     Note: EXPERIMENTAL. Hardwired to 128-in-103 interrupt range.\n");
    fprintf(stderr,
	    "  -f, --file <filename>       Download specified file"
	    " prior to debugging.\n\n");
    fprintf(stderr,
	    "  -j, --jtag <devname>        Port attached to JTAG box"
	    " (default: /dev/avrjtag).\n\n");
    fprintf(stderr,
	    "  -p, --program               Erase and program target ONLY."
	    " AVaRICE then exits.\n"
	    "                              Binary filename must be"
	    " specified with --file\n"
	    "                              option.\n\n");
    fprintf(stderr,
	    "  -e, --erase                 Erase target ONLY. AVaRICE"
	    " then exits.\n\n");
    fprintf(stderr,
	    "  -r, --read-fuses            Read fuses bytes. AVaRICE"
	    " then exits.\n\n");
    fprintf(stderr,
	    "e.g. %s  --file test.bin  --jtag /dev/ttyS0  localhost 4242\n\n",
	    progname);
    exit(1);
}

int main(int argc, char **argv)
{
    int sock;
    struct sockaddr_in clientname;
    struct sockaddr_in name;
    char *inFileName = 0;
    char *jtagDeviceName = "/dev/avrjtag";
    char *hostName = 0;
    int  hostPortNumber = 0;
    bool eraseAndQuitOnly = false;
    bool programAndQuitOnly = false;
    bool readFusesAndQuitOnly = false;
    bool detach = false;
    bool capture = false;

    statusOut("AVaRICE version %s, %s %s\n\n",
	      PACKAGE_VERSION, __DATE__, __TIME__);

    //
    // Chew over the command line.
    //
    for (int j = 1; j < argc; ++j)
    {
    	if (argv[j][0] == '-')
	{
	    if (((0 == strcmp("--file", argv[j]))  ||
		 (0 == strcmp("-f", argv[j]))) &&
		(j + 1 < argc))
	    {
		inFileName = argv[++j];
	    }
	    else if (((0 == strcmp("--jtag", argv[j]))  ||
		      (0 == strcmp("-j", argv[j]))) &&
		     (j + 1 < argc))
	    {
		jtagDeviceName = argv[++j];
	    }
	    else if (0 == strcmp("--detach", argv[j]))
	    {
	        detach = true;
	    }
	    else if (0 == strcmp("--capture", argv[j]))
	    {
	        capture = true;
	    }
	    else if (0 == strcmp("--ignore-intr", argv[j]))
	    {
	        ignoreInterrupts = true;
	    }
	    else if ((0 == strcmp("--debug", argv[j])) ||
		     (0 == strcmp("-d", argv[j])))
	    {
		debugMode = true;
	    }
	    else if ((0 == strcmp("--program", argv[j])) ||
		     (0 == strcmp("-p", argv[j])))
	    {
		programAndQuitOnly = true;
	    }
	    else if ((0 == strcmp("--erase", argv[j])) ||
		     (0 == strcmp("-e", argv[j])))
	    {
		eraseAndQuitOnly = true;
	    }
	    else if ((0 == strcmp("--read-fuses", argv[j])) ||
		     (0 == strcmp("-r", argv[j])))
            {
                readFusesAndQuitOnly = true;
            }
	    else // includes --help ...
	    {
		usage(argv[0]);
	    }
	}
	else
	{
	    if (hostName == 0)
	    {
		hostName = argv[j];
	    }
	    else if (!hostPortNumber)
	    {
		hostPortNumber = (int)strtol(argv[j],(char **)0, 0);
	    }
	    else
	    {
		usage(argv[0]);
	    }
	}
    }
    if (!hostName || hostPortNumber <= 0)
    {
	usage(argv[0]);
    }

    // And say hello to the JTAG box
    initJtagPort(jtagDeviceName);

    initJtagBox(capture);

    if (eraseAndQuitOnly)
    {
	statusOut("Erasing program memory.\n");
	enableProgramming();
	eraseProgramMemory();
	disableProgramming();
	statusOut("Erase complete.\n");

	exit(0); // All done. Bye now!
    }

    if (readFusesAndQuitOnly)
    {
	exit(0); // All done. Bye now!
    }

    if (inFileName != (char *)0)
    {
	downloadToTarget(inFileName);
	if (programAndQuitOnly)
	{
	    exit(0); // All done. Tschuss!
	}
	resetProgram();
    }
    else
    {
	check(!programAndQuitOnly,
	      "\nERROR: Filename not specified."
	      " --program must be used with --file option.\n");
    }

    initSocketAddress(&name, hostName, hostPortNumber);
    sock = makeSocket(&name, hostPortNumber);
    statusOut("Waiting for connection on port %hd.\n", hostPortNumber);
    gdbCheck(listen(sock, 1));

    if (detach)
    {
	int child = fork();

	unixCheck(child, "Failed to fork");
	if (child != 0)
	  _exit(0);
	else
	  unixCheck(setsid(), "setsid failed - weird bug");
    }

    // Connection request on original socket.
    socklen_t size = (socklen_t)sizeof(clientname);
    int gfd = accept(sock, (struct sockaddr *)&clientname, &size);
    gdbCheck(gfd);
    statusOut("Connection opened by host %s, port %hd.\n",
	      inet_ntoa(clientname.sin_addr), ntohs(clientname.sin_port));

    setGdbFile(gfd);

    // Now do the actual processing of GDB messages
    // We stay here until exiting because of error of EOF on the
    // gdb connection
    for (;;)
      talkToGdb();

    return 0;
}
