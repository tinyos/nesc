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
 * A generic list-of-element class.
 */
abstract public class NDList extends NDElement
{
    /* The elements of this list, in order. */
    public LinkedList/*NDElement*/ l = new LinkedList();

    public void child(NDElement subElement) {
	l.add(subElement);
    }
}
