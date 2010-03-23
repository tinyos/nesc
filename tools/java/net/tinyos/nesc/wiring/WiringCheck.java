// $Id$
/* 
  This file is provided under a dual BSD/GPLv2 license.  When using or 
  redistributing this file, you may do so under either license.

  GPL LICENSE SUMMARY

  Copyright(c) 2004-2005 Intel Corporation. All rights reserved.

  This program is free software; you can redistribute it and/or modify 
  it under the terms of version 2 of the GNU General Public License as
  published by the Free Software Foundation.

  This program is distributed in the hope that it will be useful, but 
  WITHOUT ANY WARRANTY; without even the implied warranty of 
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU 
  General Public License for more details.

  You should have received a copy of the GNU General Public License 
  along with this program; if not, write to the Free Software 
  Foundation, Inc., 51 Franklin St - Fifth Floor, Boston, MA 02110-1301 USA.
  The full GNU General Public License is included in this distribution 
  in the file called LICENSE.GPL.

  Contact Information:
   David Gay, david.e.gay@intel.com
   Intel Labs Berkeley, 2150 Shattuck Avenue, Suite 1300, Berkeley, CA, 94704

  BSD LICENSE 

  Copyright(c) 2004-2005 Intel Corporation. All rights reserved.
  All rights reserved.

  Redistribution and use in source and binary forms, with or without 
  modification, are permitted provided that the following conditions 
  are met:

    * Redistributions of source code must retain the above copyright 
      notice, this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above copyright 
      notice, this list of conditions and the following disclaimer in 
      the documentation and/or other materials provided with the 
      distribution.
    * Neither the name of Intel Corporation nor the names of its 
      contributors may be used to endorse or promote products derived 
      from this software without specific prior written permission.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS 
  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT 
  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR 
  A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT 
  OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, 
  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT 
  LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, 
  DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY 
  THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT 
  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE 
  OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

package net.tinyos.nesc.wiring;

import net.tinyos.nesc.dump.xml.*;
import net.tinyos.nesc.dump.*;
import java.io.*;
import java.util.*;
import org.xml.sax.*;

/* Expects an xml dump of:
     wiring 
     interfaces(!abstract())
     referenced(interfacedefs, components)
*/
public class WiringCheck
{
    final static boolean DEBUG = false;

    boolean check1Wire(WiringScan from, int min, int max) {
	int count = count(0, from);

	if (min >= 0 && count < min) 
	    return explain("underwired", from);
	if (max >= 0 && count > max)
	    return explain("overwired", from);

	return false;
    }

    boolean explain(String error, WiringScan from) {
	System.err.println(from.node.ep.location + ": interface " + from.node.ep + " " + error);
	printPath(2, from);
	return true;
    }

    /* We know the wiring graph is acyclic */

    int count(int indent, WiringScan position) {
	ListIterator out = position.edges();
	int count = 0;
	WiringScan temp = null;

	while (out.hasNext()) {
	    Xwire e = (Xwire)out.next();

	    if (temp == null)
		temp = position.duplicate();
	    else
		temp.copy(position);
	    if (temp.follow(e)) {
		if (inModule(temp))
		    count++;
		count += count(indent + 1, temp);
	    }
	}
	if (DEBUG) {
	    for (int i = 0; i < indent; i++)
		System.err.print("  ");
	    System.err.println("fcount " + count + " @ " + position);
	}

	return count;
    }

    static String repeat(int n, char c) {
	char[] cs = new char[n];

	for (int i = 0; i < n; i++)
	    cs[i] = c;

	return new String(cs);
    }

    void printPath(int offset, WiringScan position) {
	ListIterator out = position.edges();
	WiringScan temp = null;

	while (out.hasNext()) {
	    Xwire e = (Xwire)out.next();

	    if (temp == null)
		temp = position.duplicate();
	    else
		temp.copy(position);
	    if (temp.follow(e)) {
		System.err.println(repeat(offset, ' ') +
				   (position.isForwards() ? "-> " : "<- ") +
				   temp.node.ep +
				   (inModule(temp) ? " (module)" : "") +
				   " [" + e.location + "]");
		printPath(offset + 2, temp);
	    }
	}
    }

    boolean inModule(WiringEndpoint pos) {
	Xcomponent container = (Xcomponent)pos.node.ep.container;
	return container.implementation instanceof Xmodule;
    }

    void checkWiring() {
	ListIterator toCheck = Xnesc.interfaceList.listIterator();

	while (toCheck.hasNext()) {
	    int min = -1, max = -1;
	    Xinterface check1 = (Xinterface)toCheck.next();
	    Xattribute_value exactlyOnce = check1.attributeLookup("exactlyonce");
	    boolean reported = false;

	    if (check1.attributeLookup("atmostonce") != null ||
		exactlyOnce != null)
		max = 1;
	    if (check1.attributeLookup("atleastonce") != null ||
		exactlyOnce != null)
		min = 1;

	    if (min == -1 && max == -1)
		continue;

	    WiringNode checkNode = Xwiring.wg.lookup(check1);

	    if (check1.provided) {
		if (DEBUG)
		    System.err.println("P: min " + min + ", max " + max);
		reported = check1Wire(new WiringScanBackwards(checkNode), min, max);
	    }
	    else {
		if (DEBUG)
		    System.err.println("U: min " + min + ", max " + max);
		check1Wire(new WiringScanForwards(checkNode), min, max);
	    }
	}
    }

    public static void main(String[] args) throws IOException {
	try {
	    new NDReader().parse(new InputSource(System.in));
	    new WiringCheck().checkWiring();
	}
	catch (SAXException e) {
	    System.err.println("no xml reader found");
	}
    }
}
