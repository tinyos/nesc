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

import net.tinyos.nesc.dump.*;
import org.xml.sax.*;
import java.util.*;

/**
 * Base class for definitions of C objects (typedefs, variables, functions, 
 * constants, interfaces, and internal-components (component references in
 * configurations). These are uniquely identified by their 'ref' attribute.
 *
 * @author contributor: Elaine Cheong <celaine@cvs.sourceforge.net>
 */
abstract public class DataDefinition extends CDefinition
{
    /**
     * Name of this object. Not globally unique.
     */
    public String name; 
    /**
     * Unique identifier for this object.
     */
    public String ref;

    /**
     * (definition only) Type of this object.
     */
    public Type type;

    /* for reference handling */
    public void init(Attributes attrs) {
	super.init(attrs);
	ref = attrs.getValue("ref");
	name = attrs.getValue("name");
	/* ignoring scoped for now */
    }

    /* for adding new attributes (does not overwrite existing ones) */
    public void addNewAttributes(Attributes attrs) {
        super.addNewAttributes(attrs);
	if (ref == null) {
            ref = attrs.getValue("ref");
        }
	if (name == null) {
            name = attrs.getValue("name");
        }
	/* ignoring scoped for now */
    }
    
    public synchronized NDElement start(Attributes attrs) {
	return Xnesc.defsDataDefinition.define(attrs.getValue("ref"), attrs, this);
    }

    static synchronized Definition lookup(NDReader reader, Attributes attrs, 
					  String elementName) {
	return Xnesc.defsDataDefinition.lookup(reader, attrs.getValue("ref"), attrs, elementName);
    }

    public void child(NDElement subElement) {
	super.child(subElement);
	if (subElement instanceof Type)
	    type = (Type)subElement;
    }

    public String toString() {
	if (container == null)
	    return name;
	return container.toString() + "." + name;
    }

    public String debugString() {
	String base = "";
	//base += "[" + super.toString() + "]";
	if (name != null)
	    return base + "C(" + name + ", " + ref + ")";
	else
	    return  base + "C(" + ref + ")";
    }
}
