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

public class NescDefinition extends Definition
{
    static protected DefinitionTable defs = new DefinitionTable();

    public String qname; /* globally unique */

    public void init(Attributes attrs) {
	qname = attrs.getValue("qname");
    }

    public synchronized NDElement start(Attributes attrs) {
	return defs.define(attrs.getValue("qname"), attrs, this);
    }

    static synchronized Definition lookup(Attributes attrs, NDReader reader,
					  String elementName) {
	return defs.lookup(reader, attrs.getValue("qname"), attrs, elementName);
    }
}
