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

/* Expects an xml dump produced by
   -fnesc-dump=wiring Blink.nc -fnesc-dump=interfaces(attribute(atmostonce))
*/
public class WiringCheck
{
    /* Returns true if intf contains commands (when wantCommands = true)
       or events (when wantCommands = false) */
    boolean contains(Xinterface intf, boolean wantCommands) {
	Xinterfacedef idef = (Xinterfacedef)intf.instance.parent;

	return true;
    }

    void check1Wire(WiringNode from, boolean forwards, int min, int max) {
	WiringPosition pfrom = new WiringPosition(from);
	int count = forwards ? countForwards(pfrom) : countBackwards(pfrom);

	if (min >= 0 && count < min)
	    System.err.println("Interface " + from.ep.name + " underwired");
	if (max >= 0 && count > max)
	    System.err.println("Interface " + from.ep.name + " overwired");
    }
    
    /* We know the wiring graph is acyclic */

    int countForwards(WiringPosition position) {
	ListIterator out = position.node.outgoingEdges();
	int count = 0;
	WiringPosition temp = new WiringPosition();

	while (out.hasNext()) {
	    WiringEdge e = (WiringEdge)out.next();

	    temp.copy(position);
	    if (e.followForward(temp)) {
		if (inModule(temp))
		    count++;
		count += countForwards(temp);
	    }
	}
	return count;
    }

    int countBackwards(WiringPosition position) {
	ListIterator out = position.node.incomingEdges();
	int count = 0;
	WiringPosition temp = new WiringPosition();

	while (out.hasNext()) {
	    WiringEdge e = (WiringEdge)out.next();

	    temp.copy(position);
	    if (e.followBackward(temp)) {
		if (inModule(temp))
		    count++;
		count += countBackwards(temp);
	    }
	}
	return count;
    }

    boolean inModule(WiringPosition pos) {
	Xcomponent container = (Xcomponent)pos.node.ep.container;
	return container.implementation instanceof Xmodule;
    }

    void checkWiring() {
	ListIterator toCheck = Xinterfaces.list.listIterator();

	while (toCheck.hasNext()) {
	    int min = -1, max = -1;
	    Xinterface check1 = (Xinterface)toCheck.next();
	    Xattribute_value exactlyOnce = check1.attributeLookup("exactlyonce");

	    if (check1.attributeLookup("atmostonce") != null ||
		exactlyOnce != null)
		max = 1;
	    if (check1.attributeLookup("atleastonce") != null ||
		exactlyOnce != null)
		min = 1;

	    boolean providing = contains(check1, check1.provided);
	    boolean using = contains(check1, !check1.provided);

	    if (providing)
		check1Wire(Xwiring.wg.lookup(check1), true, min, max);
	    if (using)
		check1Wire(Xwiring.wg.lookup(check1), false, min, max);
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
