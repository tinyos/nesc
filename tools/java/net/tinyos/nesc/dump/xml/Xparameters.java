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
 * A function, interface or component parameter list specfication.
 */
public class Xparameters extends NDList
{
    /**
     * (valid for functions only) true if the function is a variable-argument
     * list function (e.g., printf)
     */
    public boolean varargs;

    public void child(NDElement subElement) {
	if (subElement instanceof Xvarargs)
	    varargs = true;
	else
	    super.child(subElement);
    }
    
}
