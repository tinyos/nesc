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
import java.util.*;

abstract public class Definition extends NDElement
{
    public boolean definitionAvailable;
    public LinkedList/*Xattribute_value*/ attributes;

    abstract public void init(Attributes attrs);

    public void child(NDElement subElement) {
	if (subElement instanceof Xattribute_value) {
	    if (attributes == null)
		attributes = new LinkedList();
	    attributes.add(subElement);
	}
    }

    /* Returns an attribute called name, or null for none */
    public Xattribute_value attributeLookup(String name) {
	ListIterator elems = attributes.listIterator();

	while (elems.hasNext()) {
	    Xattribute_value attr = (Xattribute_value)elems.next();

	    if (attr.attribute.name.equals(name))
		return attr;
	}
	return null;
    }
}
