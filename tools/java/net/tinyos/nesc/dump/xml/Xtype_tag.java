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

/**
 * A tagged (enum, struct, etc) type.
 */
public class Xtype_tag extends Type
{
    /**
     * The tagged type's definition.
     */
    public TagDefinition tag;

    public void child(NDElement subElement) {
	if (subElement instanceof TagDefinition)
	    tag = (TagDefinition)subElement;
    }

    public boolean equals(Object obj) {
	if (!(obj instanceof Xtype_tag))
	    return false;
	return tag == ((Xtype_tag)obj).tag;
    }
}
