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

/**
 * Base class for objects representing definitions of nesC objects
 * (typedefs, variables, components, etc). A particular kind of nesC
 * object is represented by two elements: a definition element (e.g.,
 * "interface") and a reference element (e.g., "interface-ref"). For
 * every nesC object Y, there is at most one definition element,
 * and an arbitrary number of reference elements. All elements for Y
 * resolve to the same Java object.
 * <p>
 * toString() returns a user-friendly name for the represented nesC object
 */

abstract public class Definition extends NDElement
{
    /**
     * true if the definition element for this nesC object has been
     * seen. Fields are marked with (definition only) if they are only
     * defined once the definition is available.
     */
    public boolean definitionAvailable;

    /* (definition only) The attributes on this nesC object */
    public LinkedList/*Xattribute_value*/ attributes;

    /* (definition only) The source code location where this object
       is defined. May be null. */
    public Location location;

    /**
     * init is called when the first element for this nesC object is seen
     * (it may be a reference or a definition element).
     * @param attrs Attributes from the first definition or reference element.
     */
    public void init(Attributes attrs) {
	location = Location.decode(attrs.getValue("loc"));
    }

    public void child(NDElement subElement) {
	if (subElement instanceof Xattribute_value) {
	    if (attributes == null)
		attributes = new LinkedList();
	    attributes.add(subElement);
	}
    }

    /**
     * Lookup a nesC attribute by name.
     * @param name Name of attriute to find.
     * @return An attribute in the attributes list with the specified
     *   name, or null if none is found.
     */
    public Xattribute_value attributeLookup(String name) {
	if (attributes == null)
	    return null;

	ListIterator elems = attributes.listIterator();

	while (elems.hasNext()) {
	    Xattribute_value attr = (Xattribute_value)elems.next();
	    Xattribute a = attr.attribute;
	    String n = a.name;

	    if (n.equals(name))
		return attr;
	}
	return null;
    }
}
