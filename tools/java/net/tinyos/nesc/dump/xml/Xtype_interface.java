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
 * Type of interfaces in components. Each separate interface has its own
 * separate (singleton) type (in particular, even if you, e.g., provide
 * StdControl twice, the types for the two interfaces are different).
 */
public class Xtype_interface extends Type
{
    /**
     * What interface this type is for.
     */
    public Xinterface intf;

    public void child(NDElement subElement) {
	if (subElement instanceof Xinterface)
	    intf = (Xinterface)subElement;
    }

    public boolean equals(Object obj) {
	if (!(obj instanceof Xtype_interface))
	    return false;
	return intf == ((Xtype_interface)obj).intf;
    }
}
