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

class DefinitionTable
{
    protected Hashtable allDefinitions;

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

    synchronized Definition define(Object key, Attributes attrs, Definition def) {
	Definition me = (Definition)allDefinitions.get(this);
	if (me == null) {
	    me = def;
	    me.init(attrs);
	    allDefinitions.put(key, me);
	}
	me.definitionAvailable = true;
	return me;
    }
}
