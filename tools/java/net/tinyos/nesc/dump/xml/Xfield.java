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

public class Xfield extends Definition
{
    static protected DefinitionTable defs;

    public String name; /* not unique, even within the containing struct/... */
    public String ref; /* globally unique */
    public Type type;
    public StructureDefinition container;
    public Constant bitOffset, size, bitSize;
    public boolean packed;

    public void init(Attributes attrs) {
	ref = attrs.getValue("ref");
	name = attrs.getValue("field");
    }

    public synchronized NDElement start(Attributes attrs) {
	Xfield me = (Xfield)defs.define(attrs.getValue("ref"), attrs, this);
	me.bitOffset = Constant.decode(attrs.getValue("bit-offset"));
	me.packed = boolDecode(attrs.getValue("packed"));
	String s = attrs.getValue("size");
	if (s != null)
	    size = Constant.decode(s);
	s = attrs.getValue("bit-size");
	if (s != null)
	    bitSize = Constant.decode(s);
    }

    static synchronized Definition lookup(NDReader reader, Attributes attrs) {
	return defs.lookup(reader, attrs.getValue("ref"), attrs, "field");
    }

    public void child(NDElement subElement) {
	super.child(subElement);
	if (subElement instanceof Type)
	    type = (Type)subElement;
    }
}
