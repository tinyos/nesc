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

/**
 * A node in a wiring graph.
 * @see net.tinyos.nesc.dump.xml.WiringGraph
 * @see net.tinyos.nesc.dump.xml.Xwire
 * @see net.tinyos.nesc.dump.xml.WiringEndpoint
 */
public class WiringNode
{
    static LinkedList/*Xwire*/ empty = new LinkedList();

    LinkedList/*Xwire*/ outgoing, incoming;

    /**
     * The definition this node is for.
     */
    public DataDefinition ep;

    WiringNode(DataDefinition ep) {
	this.ep = ep;
    }

    void addToEdge(Xwire e) {
	//System.err.println("" + this + " TO " + e.to);
	if (outgoing == null)
	    outgoing = new LinkedList();
	outgoing.add(e);
    }

    void addFromEdge(Xwire e) {
	//System.err.println("" + this + " FROM " + e.from);
	if (incoming == null)
	    incoming = new LinkedList();
	incoming.add(e);
    }

    /**
     * Get all outgoing edges from this node.
     * @return Outgoing edge iterator for this node.
     */
    public ListIterator/*Xwire*/ outgoingEdges() {
	if (outgoing == null)
	    return empty.listIterator();
	else
	    return outgoing.listIterator();
    }

    /**
     * Get all incoming edges from this node.
     * @return Incoming edge iterator for this node.
     */
    public ListIterator/*Xwire*/ incomingEdges() {
	if (incoming == null)
	    return empty.listIterator();
	else
	    return incoming.listIterator();
    }

    public String toString() {
	//return "(@" + super.toString() + ")node:" + ep;
	return "node:" + ep;
    }
}
