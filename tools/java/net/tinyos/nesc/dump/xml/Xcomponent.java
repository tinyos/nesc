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
 * A nesC component.
 */
public class Xcomponent extends NescDefinition implements Container
{
    /**
     * (definition only) For instances of generic components: what
     * component this is an instance of, along with the instantiation
     * arguments.  Null for non-instance components.
    */
    public Xinstance instance; 

    /**
     * (definition only) For generic components: the parameters for
     * this generic component. Null for non-generic components.
     */
    public Xparameters parameters; /* present iff component is generic */

    /**
     * (definition only) Implementation of this component. 
     */
    public Implementation implementation;

    public void child(NDElement subElement) {
	if (subElement instanceof Xinstance)
	    instance = (Xinstance)subElement;
	if (subElement instanceof Xparameters)
	    parameters = (Xparameters)subElement;
	if (subElement instanceof Implementation)
	    implementation = (Implementation)subElement;
    }

}
