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

public class Xstructured_element extends NDElement
{
    public Field field;
    public Value value;

    public NDElement start(NDReader reader, Attributes attrs) {
	field = Xfield.lookup(reader, attrs);
	return this;
    }

    public void child(NDElement subElement) {
	if (subElement instanceof Value)
	    value = (Value)subElement;
    }
}
