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
 * nesdoc element
 */
public class Xdocumentation extends NDElement
{
    /** The short documentation string for this nesdoc element */
    public String shortDoc;

    /** The long documentation string for this nesdoc element.
	May be null */
    public String longDoc;

    /** The location where this nesdoc element was found. */
    public Location location;

    public NDElement start(Attributes attrs) {
	location = Location.decode(attrs.getValue("loc"));
	return this;
    }

    public void child(NDElement subElement) {
	if (subElement instanceof Xshort)
	    shortDoc = ((Xshort)subElement).str.toString();
	if (subElement instanceof Xlong)
	    longDoc = ((Xlong)subElement).str.toString();
    }

}
