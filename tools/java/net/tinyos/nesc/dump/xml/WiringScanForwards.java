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

public class WiringScanForwards extends WiringScan
{
    public WiringScanForwards() { }

    public WiringScanForwards(WiringNode n) { 
	node = n;
    }

    public WiringScanForwards(WiringNode n, Xarguments a) { 
	node = n;
	arguments = a;
    }

    public boolean isForwards() {
	return true;
    }

    public java.util.ListIterator edges() {
	return node.outgoingEdges();
    }

    public boolean follow(Xwire e) {
	return e.followForwards(this);
    }
}
