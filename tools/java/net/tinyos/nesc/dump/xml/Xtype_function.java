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

public class Xtype_function extends Type
{
    public LinkedList/*Type*/ parameters; /* null for oldstyle */
    public Type returns;
    public boolean varargs, oldstyle;

    public NDElement start(Attributes attrs) {
	super.start(attrs);
	varargs = boolDecode(attrs.getValue("varargs"));
	oldstyle = boolDecode(attrs.getValue("oldstyle"));
	return this;
    }

    public void child(NDElement subElement) {
	if (subElement instanceof Type)
	    returns = (Type)subElement;
	if (subElement instanceof Xfunction_parameters)
	    parameters = ((Xfunction_parameters)subElement).l;
    }
}
