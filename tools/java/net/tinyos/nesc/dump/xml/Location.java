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

/**
 * Class representing source code locations.
 * <p>
 * toString() returns a user-friendly string for this location (e.g., for 
 * use in error messages).
 */
public class Location
{
    /**
     * Source code line number.
     */
    public final int lineno;

    /**
     * Source code file name.
     */
    public final String filename;

    /**
     * For elements representing items in instantiated generic
     * components: the instantiation path that led to the item being
     * created. For everything else: null.
     */
    public final String instance;

    protected static Pattern locPattern =
	Pattern.compile("([0-9]+)(\\([a-zA-Z0-9_.]+\\))?:(.*)");

    /**
     * Decode a string representing a location into a Location object.
     * @return A location object for location s. Identical source code
     *   locations may or may not be represented by the same Location object.
     */
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

	return make(lineno, filename, instance);
    }

    protected Location(int lineno, String filename, String instance) {
	this.lineno = lineno;
	this.filename = filename;
	this.instance = instance;
    }

    /** 
     * Get a source code location object. 
     * @param lineno Line number.
     * @param filename File name.
     * @param instance Generic component instance path. May be null.
     * @return A location object for the specified location. Identical
     * source code locations may or may not be represented by the same
     * Location object.
     */
    public Location make(int lineno, String filename, String instance) {
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
