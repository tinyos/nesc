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
 * Base class for definitions of C symbols (tags, variables, etc). These all
 * have a container (null for globals).
 */
abstract public class CDefinition extends Definition
{
    /**
     * (definition only) What contains this definition. Null for symbols from
     * the global scope. 
     */
    public Container container;

    public void child(NDElement subElement) {
	super.child(subElement);
	if (subElement instanceof Container)
	    container = (Container)subElement;
    }
}
