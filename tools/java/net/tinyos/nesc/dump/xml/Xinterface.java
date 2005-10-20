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

/*
@Copyright (c) 2005 The Regents of the University of California.
All rights reserved.

Permission is hereby granted, without written agreement and without
license or royalty fees, to use, copy, modify, and distribute this
software and its documentation for any purpose, provided that the
above copyright notice and the following two paragraphs appear in all
copies of this software.

IN NO EVENT SHALL THE UNIVERSITY OF CALIFORNIA BE LIABLE TO ANY PARTY
FOR DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES
ARISING OUT OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN IF
THE UNIVERSITY OF CALIFORNIA HAS BEEN ADVISED OF THE POSSIBILITY OF
SUCH DAMAGE.

THE UNIVERSITY OF CALIFORNIA SPECIFICALLY DISCLAIMS ANY WARRANTIES,
INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE. THE SOFTWARE
PROVIDED HEREUNDER IS ON AN "AS IS" BASIS, AND THE UNIVERSITY OF
CALIFORNIA HAS NO OBLIGATION TO PROVIDE MAINTENANCE, SUPPORT, UPDATES,
ENHANCEMENTS, OR MODIFICATIONS.

                                                PT_COPYRIGHT_VERSION_2
                                                COPYRIGHTENDKEY


*/

package net.tinyos.nesc.dump.xml;

import java.util.*;
import org.xml.sax.*;

/**
 * An interface used or provided by some component.
 *
 * @author contributor: Elaine Cheong <celaine@cvs.sourceforge.net>
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
        if (Xnesc.addNewAttributes) {
            me.addNewAttributes(attrs);
        }
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
