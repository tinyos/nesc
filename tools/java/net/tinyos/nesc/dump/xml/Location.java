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
    public static Location make(int lineno, String filename, String instance) {
	/* For now, not trying to do any location sharing. */
	return new Location(lineno, filename, instance);
    }

    public String toString() {
	if (instance != null)
	    return filename + "(" + instance + "):" + lineno;
	else
	    return filename + ":" + lineno;
    }
}
