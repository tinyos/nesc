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

public class Xfunction extends DataDefinition implements Container
{
    /* non-null for commands and events of interfaces of components (but
       null for those representing the command and event definition in an
       interfacedef) */
    public Xinterface intf;

    public boolean command, event;

    public NDElement start(Attributes attrs) {
	Xfunction me = (Xfunction)super.start(attrs);
	me.command = boolDecode(attrs.getValue("command"));
	me.event = boolDecode(attrs.getValue("event"));
	return me;
    }

    public void child(NDElement subElement) {
	/* Intercept references to a containing interface before we call
	   super.child, as this is not the actual container for this
	   function (the container is the component, not the interface) */
	if (subElement instanceof Xinterface)
	    intf = (Xinterface)intf;
	else
	    super.child(subElement);
    }
}
