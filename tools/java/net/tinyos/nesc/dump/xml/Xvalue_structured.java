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
import java.util.*;

/**
 * An initialiser for a structure.
 */
public class Xvalue_structured extends Value
{
    /**
     * The initialisers for the structure's fields. Note that not all
     * fields need be initialised.
     */
    public LinkedList/*Xstructured_element*/ fields = new LinkedList();

    public void child(NDElement subElement) {
	super.child(subElement);
	if (subElement instanceof Xstructured_element)
	    fields.add(subElement);
    }

    public boolean equals(Object obj) {
	return false;
    }
}
