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
 * An edge in a wiring graph.
 * @see net.tinyos.nesc.dump.xml.WiringGraph
 * @see net.tinyos.nesc.dump.xml.WiringEndpoint
 * @see net.tinyos.nesc.dump.xml.WiringNode
 */
public class Xwire extends NDElement
{
    /**
     * The origin of this wire.
     */
    public WiringEndpoint from;

    /**
     * The destination of this wire.
     */
    public WiringEndpoint to;

    /**
     * The source code location which created this wire. null in
     * function-level wiring graphs.
     */
    public Location location;

    public NDElement start(Attributes attrs) {
	location = Location.decode(attrs.getValue("loc"));
	return this;
    }

    public void child(NDElement subElement) {
	super.child(subElement);
	if (subElement instanceof Xfrom)
	    from = (Xfrom)subElement;
	if (subElement instanceof Xto)
	    to = (Xto)subElement;
    }

    protected boolean follow(WiringEndpoint position,
			     WiringEndpoint start, WiringEndpoint end) {
	/* assert(position.node == start.node); */
	if (position.arguments != null) {
	    if (start.arguments != null) {
		if (!start.arguments.equals(position.arguments))
		    return false;
		position.arguments = end.arguments;
	    }
	    /* else assert(end.arguments == null); */
	}
	else
	    position.arguments = end.arguments;
	position.node = end.node;
	return true;
    }


    /**
     * Try to follow this edge forwards from 'position'. Return true if
     * this edge can be followed. If the edge can be followed, update
     * 'position' to reflect the results of following the edge.
     * <p>
     * The position's node must match this edge's starting node ('from' 
     * field).
     * <p>
     * An edge cannot be followed if 'position' specifies a particular
     * interface from a parameterised interface (i.e., has arguments),
     * and this edge specifies a different interface (i.e., has different
     * arguments in its 'from' endpoint).
     */
    public boolean followForwards(WiringEndpoint position) {
	return follow(position, from, to);
    }

    /**
     * Try to follow this edge backwards from 'position'. Return true if
     * this edge can be followed. If the edge can be followed, update
     * 'position' to reflect the results of following the edge.
     * <p>
     * The position's node must match this edge's ending node ('to' field).
     * <p>
     * An edge cannot be followed if 'position' specifies a particular
     * interface from a parameterised interface (i.e., has arguments),
     * and this edge specifies a different interface (i.e., has different
     * arguments in its 'to' endpoint).
     */
    public boolean followBackwards(WiringEndpoint position) {
	return follow(position, to, from);
    }
}
