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

/**
 * Base class for nesC definitions (components, interface definitions)
 */
abstract public class NescDefinition extends Definition
{
    static protected DefinitionTable defs = new DefinitionTable();

    /**
     * Name of this nesC component or interface definition. For instances
     * of generic components, this is the full instantiation path. Globally
     * unique.
     */
    public String qname;

    public void init(Attributes attrs) {
	super.init(attrs);
	qname = attrs.getValue("qname");
    }

    public synchronized NDElement start(Attributes attrs) {
	return defs.define(attrs.getValue("qname"), attrs, this);
    }

    static synchronized Definition lookup(Attributes attrs, NDReader reader,
					  String elementName) {
	return defs.lookup(reader, attrs.getValue("qname"), attrs, elementName);
    }

    public String toString() {
	return qname;
    }
}
