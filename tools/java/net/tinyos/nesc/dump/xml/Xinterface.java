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

import java.util.*;
import org.xml.sax.*;

/**
 * An interface used or provided by some component.
 */
public class Xinterface extends DataDefinition
{
    /**
     * (definition only) true if the interface is provided.
     */
    public boolean provided;

    /**
     * (definition only) What interface definition this interface is
     * an instance of.
     */
    public Xinstance instance;

    /**
     * (definition only) Parameters for paramterised interfaces. Null
     * otherwise.
     */
    public LinkedList/*Type*/ parameters;

    /**
     * (definition only) Functions of this interface.
     */
    public LinkedList/*Xfunction*/ functions;

    public NDElement start(Attributes attrs) {
	
    NDElement temp = super.start(attrs);
    Xinterface me = (Xinterface) temp;
	me.provided = attrs.getValue("provided").equals("1");
	return me;
    }

    public void child(NDElement subElement) {
	super.child(subElement);
	if (subElement instanceof Xinstance)
	    instance = (Xinstance)subElement;
	if (subElement instanceof Xinterface_parameters)
	    parameters = ((Xinterface_parameters)subElement).l;
	if (subElement instanceof Xinterface_functions)
	    functions = ((Xinterface_functions)subElement).l;
    }
}
