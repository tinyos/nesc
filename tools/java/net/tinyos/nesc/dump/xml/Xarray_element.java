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
 * Elements of an array initialiser (Xvalue_array)
 * @see net.tinyos.nesc.dump.xml.Xvalue_array
 */
public class Xarray_element extends NDElement
{
    /**
     * First array element initialised with this value 
     */
    public long from;

    /**
     * Last array element initialised with this value 
     */
    public long to;

    /**
     * Value placed in these array elements
     */
    public Value value;

    public NDElement start(Attributes attrs) {
	from = numberDecode(attrs.getValue("from"), -1);
	to = numberDecode(attrs.getValue("to"), -1);
	return this;
    }

    public void child(NDElement subElement) {
	if (subElement instanceof Value)
	    value = (Value)subElement;
    }
}
