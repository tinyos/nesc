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

/**
 * Base class for scanning wiring graphs for forwards or backwards
 */
abstract public class WiringScan extends WiringEndpoint implements Cloneable
{
    /**
     * Return the edges in the specified graph from this endpoint
     * @return The list iterator for these edges. 
     */
    abstract public java.util.ListIterator edges();

    /**
     * Follow edge 'e', modifying this endpoint to represent arrival point.
     * @param e Edge to follow.
     * @return true if the edge could be followed ('this' modified to 
     *   reflect destination), false if the edge could not be followed
     *   ('this' not modified).
     */
    abstract public boolean follow(Xwire e);

    /**
     * Is this a forwards or backwards scanner
     * @return true for forwards scanners, false for backwards ones.
     */
    abstract public boolean isForwards();

    /**
     * Create a new wiring scanner at the same position in the wiring graph,
     * scanning in the same direction.
     * @return The new scanner.
     */
    public WiringScan duplicate() {
	try {
	    return (WiringScan)clone();
	}
	catch (CloneNotSupportedException e) { 
	    return null; // cannot happen
	}
    }
}
