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
 * Information on instances of interfaces and generic components.
 */
public class Xinstance extends NDElement
{
    /**
     * For instances of generic components: a unique number identifying
     * this particular instance. These numbers start consecutively at 0.
     * -1 for instances of interface definitions.
     */
    public long number;

    /**
     * Arguments for this instance. null for instances of non-generic
     * interfaces.
     */
    public Xarguments arguments;

    /**
     * What component or interface this is an instance of.
     */
    public NescDefinition parent;

    public NDElement start(Attributes attrs) {
	number = numberDecode(attrs.getValue("number"), -1);
	return this;
    }

    public void child(NDElement subElement) {
	if (subElement instanceof Xarguments)
	    arguments = (Xarguments)subElement;
	if (subElement instanceof NescDefinition)
	    parent = (NescDefinition)subElement;
    }
}
