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
import net.tinyos.nesc.dump.*;

/**
 * A C typedef.
 */
public class Xtypedef extends DataDefinition
{
    public NDElement start(NDReader reader, Attributes attrs) {
	if (attrs.getValue("ref") == null) {
	    /* work around nesC 1.2.1 bug. This should really be a typename */
	    try {
		NDElement me = reader.makeElement("typename");
		return me.start(reader, attrs);
	    }
	    catch (Exception e) { /* stuff is broken */ }
	}
	return super.start(reader, attrs);
    }

}
