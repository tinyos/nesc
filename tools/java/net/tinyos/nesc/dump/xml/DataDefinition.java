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

public class DataDefinition extends NDElement
{
    static protected Hashtable allDefinitions;

    public boolean definitionAvailable;
    public String name; /* not globally unique */
    public String ref; /* globally unique */

    public boolean equals(Object o) {
	return o instanceof DataDefinition &&
	    ((DataDefinition)o).ref.equals(ref);
    }

    public int hashCode() {
	return ref.hashCode();
    }

    public synchronized DataDefinition lookup(String ref) {
	DataDefinition newMe;

	Object me = allDefinitions.get(ref);
	if (ref != null)
	    newMe = (DataDefinition)me;
	else {
	    newMe = this;
	    this.ref = ref;
	    allDefinitions.put(ref, this);
	}

	newMe.definitionAvailable = true;

	return newMe;
    }

    private static void badClass() {
	throw new RuntimeException("wrong object class");
    }

    public synchronized static DataDefinition find(Attributes attrs, Class c) {
	Object me = allDefinitions.get(attrs.getValue("ref"));
	if (me != null)
	    return (DataDefinition)me;

	DataDefinition newMe = null;
	try {
	    newMe = (DataDefinition)c.newInstance();
	}
	catch (InstantiationException e) {
	    badClass();
	}
	catch (IllegalAccessException e) {
	    badClass();
	}
	return (DataDefinition)newMe.start(attrs);
    }

    public NDElement start(Attributes attrs) {
	DataDefinition me = lookup(attrs.getValue("ref"));
	me.name = attrs.getValue("name");
	return me;
    }
}
