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

import net.tinyos.nesc.dump.*;
import org.xml.sax.*;
import java.util.*;

public class DataDefinition extends CDefinition
{
    static protected DefinitionTable defs = new DefinitionTable();

    public String name; /* not globally unique */
    public String ref; /* globally unique */
    public Type type;

    /* for reference handling */
    public void init(Attributes attrs) {
	super.init(attrs);
	ref = attrs.getValue("ref");
	name = attrs.getValue("name");
	/* ignoring scoped for now */
    }

    public synchronized NDElement start(Attributes attrs) {
	return defs.define(attrs.getValue("ref"), attrs, this);
    }

    static synchronized Definition lookup(NDReader reader, Attributes attrs, 
					  String elementName) {
	return defs.lookup(reader, attrs.getValue("ref"), attrs, elementName);
    }

    public void child(NDElement subElement) {
	super.child(subElement);
	if (subElement instanceof Type)
	    type = (Type)subElement;
    }

    public String toString() {
	if (container == null)
	    return name;
	return container.toString() + "." + name;
    }

    public String debugString() {
	String base = "";
	//base += "[" + super.toString() + "]";
	if (name != null)
	    return base + "C(" + name + ", " + ref + ")";
	else
	    return  base + "C(" + ref + ")";
    }
}
