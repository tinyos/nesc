// $Id$
/* 
  This file is provided under a dual BSD/GPLv2 license.  When using or 
  redistributing this file, you may do so under either license.

  GPL LICENSE SUMMARY

  Copyright(c) 2004-2005 Intel Corporation. All rights reserved.

  This program is free software; you can redistribute it and/or modify 
  it under the terms of version 2 of the GNU General Public License as
  published by the Free Software Foundation.

  This program is distributed in the hope that it will be useful, but 
  WITHOUT ANY WARRANTY; without even the implied warranty of 
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU 
  General Public License for more details.

  You should have received a copy of the GNU General Public License 
  along with this program; if not, write to the Free Software 
  Foundation, Inc., 51 Franklin St - Fifth Floor, Boston, MA 02110-1301 USA.
  The full GNU General Public License is included in this distribution 
  in the file called LICENSE.GPL.

  Contact Information:
   David Gay, david.e.gay@intel.com
   Intel Labs Berkeley, 2150 Shattuck Avenue, Suite 1300, Berkeley, CA, 94704

  BSD LICENSE 

  Copyright(c) 2004-2005 Intel Corporation. All rights reserved.
  All rights reserved.

  Redistribution and use in source and binary forms, with or without 
  modification, are permitted provided that the following conditions 
  are met:

    * Redistributions of source code must retain the above copyright 
      notice, this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above copyright 
      notice, this list of conditions and the following disclaimer in 
      the documentation and/or other materials provided with the 
      distribution.
    * Neither the name of Intel Corporation nor the names of its 
      contributors may be used to endorse or promote products derived 
      from this software without specific prior written permission.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS 
  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT 
  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR 
  A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT 
  OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, 
  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT 
  LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, 
  DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY 
  THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT 
  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE 
  OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
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
