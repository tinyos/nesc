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

import java.util.regex.*;

public class Location
{
    public int lineno;
    public String filename;
    public String instance; /* may be null */

    protected static Pattern locPattern =
	Pattern.compile("([0-9]+)(\\([a-zA-Z0-9_.]+\\))?:(.*)");

    public static Location decode(String s) {
	if (s == null)
	    return null;

	Matcher m = locPattern.matcher(s);
	if (!m.matches())
	    return null;

	int lineno;
	String numS = m.group(1);
	String instance = m.group(2);
	String filename = m.group(3);

	try {
	    lineno = Integer.decode(numS).intValue();
	}
	catch (NumberFormatException e) {
	    return null;
	}

	return new Location(lineno, filename, instance);
    }

    public Location(int lineno, String filename, String instance) {
	this.lineno = lineno;
	this.filename = filename;
	this.instance = instance;
    }

    public String toString() {
	if (instance != null)
	    return filename + "(" + instance + "):" + lineno;
	else
	    return filename + ":" + lineno;
    }
}
