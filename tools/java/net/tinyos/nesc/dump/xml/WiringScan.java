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

abstract public class WiringScan extends WiringEndpoint implements Cloneable
{
    abstract public java.util.ListIterator edges();
    abstract public boolean follow(Xwire e);
    abstract public boolean isForwards();

    public WiringScan duplicate() {
	try {
	    return (WiringScan)clone();
	}
	catch (CloneNotSupportedException e) { 
	    return null; // cannot happen
	}
    }
}
