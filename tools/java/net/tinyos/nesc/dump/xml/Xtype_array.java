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
 * A C array type.
 */
public class Xtype_array extends Type
{
    /**
     * Base type for the array.
     */
    public Type subType;

    /**
     * Length of the array. Might be a non-constant for arrays of unspecified
     * length.
     */
    public Constant length;

    public NDElement start(Attributes attrs) {
	super.start(attrs);
	length = Constant.decode(attrs.getValue("elements"));
	return this;
    }

    public void child(NDElement subElement) {
	if (subElement instanceof Type)
	    subType = (Type)subElement;
    }

    public boolean equals(Object obj) {
	if (!(obj instanceof Xtype_array))
	    return false;
	Xtype_array other = (Xtype_array)obj;
	return subType.equals(other.subType) &&
	    ((!length.constant() && !other.length.constant()) ||
	     length.equals(other.length));
    }
}
