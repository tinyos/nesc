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

public class Xtype_component extends Type
{
    public Xinternal_component component;

    public void child(NDElement subElement) {
	if (subElement instanceof Xinternal_component)
	    component = (Xinternal_component)subElement;
    }

    public boolean equals(Object obj) {
	if (!(obj instanceof Xtype_component))
	    return false;
	return component == ((Xtype_component)obj).component;
    }
}
