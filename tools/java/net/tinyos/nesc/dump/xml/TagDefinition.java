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

public class TagDefinition extends CDefinition
{
    static protected DefinitionTable defs;

    public String name; /* not globally unique, may be null */
    public String ref; /* globally unique */
    public Constant size, alignment;
    public boolean defined, packed;

    public void init(Attributes attrs) {
	ref = attrs.getValue("ref");
	name = attrs.getValue("name");
	/* ignoring scoped for now */
    }

    public synchronized NDElement start(Attributes attrs) {
	TagDefinition me = (TagDefinition)defs.define(attrs.getValue("ref"), attrs, this);
	me.size = Constant.decode(attrs.getValue("size"));
	me.alignment = Constant.decode(attrs.getValue("alignment"));
	me.defined = boolDecode(attrs.getValue("defined"));
	me.packed = boolDecode(attrs.getValue("packed"));
	return me;
    }

    static synchronized Definition lookup(NDReader reader, Attributes attrs, 
					  String elementName) {
	return defs.lookup(reader, attrs.getValue("ref"), attrs, elementName);
    }
}
