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
 * Base class for Java object representing a C tag (enum, struct, union)
 */
abstract public class TagDefinition extends CDefinition
{
    static protected DefinitionTable defs = new DefinitionTable();

    /**
     * Name of this tag, not globally unique, may be null.
     */
    public String name;

    /**
     * Unique identifier for this tag.
     */
    public String ref;

    /**
     * (definition only) Size of objects of this struct/union/enum/etc.
     */
    public Constant size;

    /**
     * (definition only) Alignment for objects of this struct/union/enum/etc.
     */
    public Constant alignment;

    /**
     * (definition only) True if this tag is actually defined.
     */
    public boolean defined;

    /**
     * (definition only) True if the gcc packed "attribute" was used on
     * this tag.
     */
    public boolean packed;

    public void init(Attributes attrs) {
	super.init(attrs);
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
