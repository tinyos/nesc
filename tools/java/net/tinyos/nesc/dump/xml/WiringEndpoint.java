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

import org.xml.sax.*;

/**
 * A position in a wiring graph, i.e., a node and an optional argument list.
 *
 * @see net.tinyos.nesc.dump.xml.WiringGraph
 * @see net.tinyos.nesc.dump.xml.WiringNode
 * @see net.tinyos.nesc.dump.xml.Xwire
 */
public class WiringEndpoint extends NDElement
{
    /**
     * The graph node of this endpoint.
     */
    public WiringNode node;

    /**
     * Arguments to the graph node for this endpoint. May be null.
     */
    public Xarguments arguments;

    public void child(NDElement subElement) {
	if (subElement instanceof DataDefinition)
	    node = new WiringNode((DataDefinition)subElement);
	if (subElement instanceof Xarguments)
	    arguments = (Xarguments)subElement;
    }
    
    /**
     * Create a new, unitialised wiring endpoint.
     */
    public WiringEndpoint() { }

    /**
     * Create a new wiring endpoint on wiring graph node n.
     * @param n Wiring node graph to create endpoint for.
     */
    public WiringEndpoint(WiringNode n) { 
	node = n;
    }

    /**
     * Create a new wiring endpoint on wiring graph node n, with arguments a.
     * @param n Wiring node graph to create endpoint for.
     * @param a Arguments for endpoint.
     */
    public WiringEndpoint(WiringNode n, Xarguments a) { 
	node = n;
	arguments = a;
    }

    /**
     * Create a new wiring endpoint as a copy of endpoint p.
     * @param p endpoint to copy.
     */
    public WiringEndpoint(WiringEndpoint p) { 
	copy(p);
    }
    
    /**
     * Copy endpoint from into this endpoint.
     * @param from endpoint to copy.
     */
    public void copy(WiringEndpoint from) {
	node = from.node;
	arguments = from.arguments;
    }

    public String toString() {
	return node.toString();
    }
}
