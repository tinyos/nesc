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

    public boolean followForwards(WiringPosition position) {
	if (position.args != null) {
	    if (fromArgs != null) {
		if (!fromArgs.equals(position.args))
		    return false;
		position.args = toArgs;
	    }
	    /* else assert(toArgs == null); */
	}
	else
	    position.args = toArgs;
	position.node = to;
	return true;
    }

    public boolean followBackwards(WiringPosition position) {
	if (position.args != null) {
	    if (toArgs != null) {
		if (!toArgs.equals(position.args))
		    return false;
		position.args = fromArgs;
	    }
	    /* else assert(fromArgs == null); */
	}
	else
	    position.args = fromArgs;
	position.node = from;
	return true;
    }
}
