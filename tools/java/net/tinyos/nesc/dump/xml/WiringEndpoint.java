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

public class WiringEndpoint extends NDElement
{
    public WiringNode node;
    public Xarguments arguments; /* optional */

    public void child(NDElement subElement) {
	if (subElement instanceof DataDefinition)
	    node = new WiringNode((DataDefinition)subElement);
	if (subElement instanceof Xarguments)
	    arguments = (Xarguments)subElement;
    }
    
    public WiringEndpoint() { }

    public WiringEndpoint(WiringNode n) { 
	node = n;
    }

    public WiringEndpoint(WiringNode n, Xarguments a) { 
	node = n;
	arguments = a;
    }

    public WiringEndpoint(WiringEndpoint p) { 
	copy(p);
    }
    
    public void copy(WiringEndpoint from) {
	node = from.node;
	arguments = from.arguments;
    }

    public String toString() {
	return node.toString();
    }
}
