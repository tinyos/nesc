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

import net.tinyos.nesc.dump.*;
import org.xml.sax.*;

abstract public class NDElement {
    public NDElement start(Attributes attrs) {
	return this;
    }

    public NDElement start(NDReader reader, Attributes attrs) {
	return start(attrs);
    }

    public void child(NDElement subElement) {
    }

    public void child(NDReader reader, NDElement subElement) {
	child(subElement);
    }

    public NDElement end() {
	return this;
    }

    public NDElement end(NDReader reader) {
	return end();
    }

    public void characters(char[] ch, int start, int length) {
    }

    public void whitespace() {
    }


    static public long numberDecode(String s, long def) {
	if (s != null) {
	    try {
		return Long.decode(s).longValue();
	    }
	    catch (NumberFormatException e) { }
	}
	return def;
    }

    static public double realDecode(String s, double def) {
	if (s != null) {
	    try {
		return Double.valueOf(s).doubleValue();
	    }
	    catch (NumberFormatException e) { }
	}
	return def;
    }

    static public boolean boolDecode(String s) {
	return s != null;
    }
}
