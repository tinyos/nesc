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

package net.tinyos.nesc.dump.xml;

import java.util.*;

public class WiringNode
{
    LinkedList outgoing, incoming;
    public DataDefinition ep;

    WiringNode(DataDefinition ep) {
	this.ep = ep;
    }

    void addToEdge(WiringEdge e) {
	if (outgoing == null)
	    outgoing = new LinkedList();
	outgoing.add(e);
    }

    void addFromEdge(WiringEdge e) {
	if (incoming == null)
	    incoming = new LinkedList();
	incoming.add(e);
    }

    public ListIterator outgoingEdges() {
	return outgoing.iterator();
    }

    public ListIterator incomingEdges() {
	return incoming.iterator();
    }
}
