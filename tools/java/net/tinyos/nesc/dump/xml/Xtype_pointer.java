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
 * A pointer type.
 */
public class Xtype_pointer extends Type
{
    /**
     * The type pointed to.
     */
    public Type subType;

    public void child(NDElement subElement) {
	if (subElement instanceof Type)
	    subType = (Type)subElement;
    }

    public boolean equals(Object obj) {
	if (!(obj instanceof Xtype_pointer))
	    return false;
	return subType.equals(((Xtype_array)obj).subType);
    }
}
