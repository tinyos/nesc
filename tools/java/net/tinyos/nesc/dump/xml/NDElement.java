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

/**
 * Base class for all Java representations of nesC dump XML elements.
 * All Xbaz classes representing XML element baz must inherit from NDElement.
 * The methods of NDElement are called at various stages of XML parsing for
 * baz (see method descriptions below).
 *
 * @see net.tinyos.nesc.dump.NDReader
 * @author David Gay
 */
abstract public class NDElement {
    /**
     * Called after element creation if 
     *  start(NDReader reader, Attributes attrs) 
     * is not overridden. Returned object will represent this element 
     * during parsing.
     * 
     * @param attrs Attributes for this element.
     * @return The object which will represent this element in the created
     *   data structure.
     */
    public NDElement start(Attributes attrs) {
	return this;
    }

    /**
     * Called after element creation. Returned object will represent
     * this element during parsing.
     * 
     * @param reader The current nesC dump reader.
     * @param attrs Attributes for this element.
     * @return The object which will represent this element in the created
     *   data structure.
     */
    public NDElement start(NDReader reader, Attributes attrs) {
	return start(attrs);
    }

    /**
     * Called after each subelement has been parsed if 
     *  void child(NDReader reader, NDElement subElement)
     * is not overridden. 
     * 
     * @param subElement Last subelement parsed.
     */
    public void child(NDElement subElement) {
    }

    /**
     * Called after each subelement has been parsed.
     * 
     * @param subElement Last subelement parsed.
     */
    public void child(NDReader reader, NDElement subElement) {
	child(subElement);
    }

    /**
     * Called after the element has been fully parsed if 
     *  end(NDReader reader) 
     * is not overridden. Returned object will be passed to the parent
     * element's child method. 
     * 
     * @param attrs Attributes for this element.
     * @return The object which will represent this element in the created
     *   data structure.
     */
    public NDElement end() {
	return this;
    }

    /**
     * Called after the element has been fully parsed. Returned object will
     * be passed to the parent element's child method (Note: the 
     * reader.parent() method does not work if called from end()).
     * 
     * @return The object which will be passed to the parent's child() method.
     */
    public NDElement end(NDReader reader) {
	return end();
    }

    /**
     * Called after each character sequence from the XML element is parsed.
     * 
     * @param ch XML characters read.
     * @param start First valid offset in ch
     * @param length Number of characters valid in ch
     */
    public void characters(char[] ch, int start, int length) {
    }

    /**
     * Called after each whitespace sequence from the XML element is parsed.
     */
    public void whitespace() {
    }


    /**
     * Utility function to decode a string containing an integer.
     * @param s String to decode
     * @param def Default value
     * @return Decoded string value, or def if a parsing error occurs.
     */
    static public long numberDecode(String s, long def) {
	if (s != null) {
	    try {
		return Long.decode(s).longValue();
	    }
	    catch (NumberFormatException e) { }
	}
	return def;
    }

    /**
     * Utility function to decode a string containing a real number.
     * @param s String to decode
     * @param def Default value
     * @return Decoded string value, or def if a parsing error occurs.
     */
    static public double realDecode(String s, double def) {
	if (s != null) {
	    try {
		return Double.valueOf(s).doubleValue();
	    }
	    catch (NumberFormatException e) { }
	}
	return def;
    }

    /**
     * Utility function to decode a boolean attribute (boolean attributes
     * are represented by an empty string if the boolean is true, and no
     * attribute at all if the boolean is false).
     * <p>
     * Typical usage is 'boolDecode(attrs.getAttr("myboolean"))'.
     * @param s String to decode
     * @return true if s is not null, false otherwise
     */
    static public boolean boolDecode(String s) {
	return s != null;
    }
}
