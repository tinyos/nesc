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
