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

public class WiringEdge
{
    public WiringNode from, to;
    public Xarguments fromArgs, toArgs;

    WiringEdge(WiringNode from, Xarguments fromArgs,
	       WiringNode to, Xarguments toArgs) {
	this.from = from;
	this.to = to;
	this.fromArgs = fromArgs;
	this.toArgs = toArgs;
    }

    boolean followForward(WiringPosition position) {
	if (position.arguments != null) {
	    if (fromArgs != null) {
		if (!compareArguments(fromArgs, position.arguments))
		    return false;
		position.arguments = toArgs;
	    }
	    /* else assert(toArgs == null); */
	}
	else
	    position.arguments = toArgs;
	position.node = to;
	return true;
    }

    boolean followBackward(WiringPosition position) {
	if (position.arguments != null) {
	    if (toArgs != null) {
		if (!compareArguments(toArgs, position.arguments))
		    return false;
		position.arguments = fromArgs;
	    }
	    /* else assert(fromArgs == null); */
	}
	else
	    position.arguments = fromArgs;
	position.node = from;
	return true;
    }
}
