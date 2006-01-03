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
 * Type of internal component references in configurations. Each component
 * in a configuration has its own separate (singleton) type.
 */
public class Xtype_component extends Type
{
    /**
     * What internal component this is the type of.
     */
    public Xinternal_component component;

    public void child(NDElement subElement) {
	if (subElement instanceof Xinternal_component)
	    component = (Xinternal_component)subElement;
	super.child(subElement);
    }

    public boolean equals(Object obj) {
	if (!(obj instanceof Xtype_component))
	    return false;
	return component == ((Xtype_component)obj).component;
    }
}
