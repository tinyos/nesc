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

public class Xwire extends NDElement
{
    public WiringEndpoint from, to;
    public Location location; /* may be null */

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


    public boolean followForwards(WiringEndpoint position) {
	return follow(position, from, to);
    }

    public boolean followBackwards(WiringEndpoint position) {
	return follow(position, to, from);
    }
}
