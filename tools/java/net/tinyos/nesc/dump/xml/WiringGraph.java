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
 * A nesC wiring graph. A slightly unconventional graph where the nodes
 * (WiringNode) may represent one or more actual graph nodes (in the case
 * where the node corresponds to a parameterised function or interface).
 * <p>
 * A position in this graph is represented by an "endpoint", which is a
 * WiringNode plus optional arguments (the optional arguments identify a
 * particular interface or function on nodes representing parameterised
 * functions or interfaces). Note that an endpoint may itself represent
 * multiple nodes in the case where the node is parameterised and the
 * endpoint has no arguments.
 * <p>
 * Edges in this graphs (Xwire) connect two endpoints.
 *
 * @see net.tinyos.nesc.dump.xml.WiringNode
 * @see net.tinyos.nesc.dump.xml.Xwire
 * @see net.tinyos.nesc.dump.xml.WiringEndpoint
 */
public class WiringGraph
{
    protected Hashtable endpoints = new Hashtable();

    /**
     * Find the node for a particular definition. Adds a new node if 
     * the definition is not found.
     * @param epDecl Node to lookup.
     * @return WiringNode for epDecl.
     */
    public WiringNode lookup(DataDefinition epDecl) {
	WiringNode found = (WiringNode)endpoints.get(epDecl);

	if (found == null) {
	    found = new WiringNode(epDecl);
	    endpoints.put(epDecl, found);
	}
	return found;
    }

    /**
     * Add edge to the graph.
     * @param wire Edge to add.
     */
    public void addEdge(Xwire wire) {
	wire.from.node = lookup(wire.from.node.ep);
	wire.from.node.addToEdge(wire);

	wire.to.node = lookup(wire.to.node.ep);
	wire.to.node.addFromEdge(wire);
    }
}
