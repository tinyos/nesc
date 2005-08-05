// $Id$
/*									tab:4
 * Copyright (c) 2004-2005 Intel Corporation
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached INTEL-LICENSE     
 * file. If you do not find these files, copies can be found by writing to
 * Intel Research Berkeley, 2150 Shattuck Avenue, Suite 1300, Berkeley, CA, 
 * 94704.  Attention:  Intel License Inquiry.
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
    /* Returns true if intf contains commands (when wantCommands = true)
       or events (when wantCommands = false) */
    boolean contains(Xinterface intf, boolean wantCommands) {
	Xinterfacedef idef = (Xinterfacedef)intf.instance.parent;
	ListIterator fns = idef.functions.listIterator();

	while (fns.hasNext()) {
	    Xfunction fn = (Xfunction)fns.next();

	    if (fn.command == wantCommands)
		return true;
	}
	return false;
    }

    boolean check1Wire(WiringScan from, int min, int max) {
	int count = count(from);

	if (min >= 0 && count < min) 
	    return explain("underwired", from);
	if (max >= 0 && count > max)
	    return explain("overwired", from);

	return false;
    }

    boolean explain(String error, WiringScan from) {
	System.err.println("Interface " + from.node.ep + " " + error);
	printPath(2, from);
	return true;
    }

    /* We know the wiring graph is acyclic */

    int count(WiringScan position) {
	ListIterator out = position.edges();
	int count = 0;
	WiringScan temp = null;

	//System.err.println("fcount " + count + " @ " + position);

	while (out.hasNext()) {
	    Xwire e = (Xwire)out.next();

	    if (temp == null)
		temp = position.duplicate();
	    else
		temp.copy(position);
	    if (temp.follow(e)) {
		if (inModule(temp))
		    count++;
		count += count(temp);
	    }
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
	ListIterator toCheck = Xinterfaces.list.listIterator();

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

	    boolean providing = contains(check1, check1.provided);
	    boolean using = contains(check1, !check1.provided);
	    WiringNode checkNode = Xwiring.wg.lookup(check1);

	    if (providing)
		reported = check1Wire(new WiringScanForwards(checkNode), min, max);
	    if (using && !reported)
		check1Wire(new WiringScanBackwards(checkNode), min, max);
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
