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
 * Internal class for matching references to definitions.
 */
class DefinitionTable
{
    protected Hashtable allDefinitions = new Hashtable();

    /**
     * Reference element found. Lookup object table to see if the referenced
     * object already exists. If not, create an object of class 'elementName'
     * call it's 'init' method, passing 'attrs' as its arguments, and add it
     * to the object table.
     * @param reader The current NDReader.
     * @param key The key uniquely identifying the nesC object (typically
     *   a string).
     * @param attrs Attributes from the reference element.
     * @param elementName Name of the definition element for this object.
     * @return The Java object for this nesC object.
     */
    synchronized Definition lookup(NDReader reader, Object key,
				   Attributes attrs, String elementName) {
	Definition me = (Definition)allDefinitions.get(key);
	if (me == null) {
	    try {
		me = (Definition)reader.makeElement(elementName);
	    }
	    catch (Exception e) {
		throw new RuntimeException("wrong object class");
	    }
	    me.init(attrs);
	    allDefinitions.put(key, me);
	}
	return me;
    }

    /**
     * Definition element found. Lookup object table to see if the
     * defined object already exists. If found, just return that
     * object. If not, add 'def' as the representative for this nesC
     * object and call 'def.init(attrs)'.  In both cases, set the
     * 'definitionAvailable' for the object to true.
     * @param key The key uniquely identifying the nesC object (typically
     *   a string).
     * @param attrs Attributes from the definition element.
     * @param def Java object created for the definition element.
     * @return The Java object for this nesC object.
     */
    synchronized Definition define(Object key, Attributes attrs, Definition def) {
	Definition me = (Definition)allDefinitions.get(key);
	if (me == null) {
	    me = def;
	    me.init(attrs);
	    allDefinitions.put(key, me);
	}
	me.definitionAvailable = true;
	return me;
    }
}
