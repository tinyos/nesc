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

/**
 * Base class for nesC constants. nesC constants are encoded in attribute
 * strings. Note that there are representations for "unknown constant" and
 * "non constant" values.
 * <p>
 * equals() is defined on Constant objects. Unknown and non constant
 * objects compare different with every other constant (including themselves).
 * <p>
 * Note that constants (ex: 32, "fun", 2+3) are different from C initialisers
 * (ex: { 1, 2, { .x = 3 } }). An initialiser is represented by an object of
 * type Value. Constants can appear in initialisers (see Xvalue). 
 * @see net.tinyos.nesc.dump.xml.Value
 * @see net.tinyos.nesc.dump.xml.Xvalue
 */
abstract public class Constant
{
    /**
     * Decode a nesC constant string.
     * @param s String to decode.
     * @return An object representing the constant encoded by s
     */
    public static Constant decode(String s) {
	/* Constants un in generic components don't have a known value */
	if (s == null)
	    return new UnknownConstant();

	switch (s.charAt(0)) {
	case 'I': return new IntegerConstant(s);
	case 'F': return new FloatConstant(s);
	case 'S': return new StringConstant(s);
	case 'V': return new NonConstant();
	case 'U': default: return new UnknownConstant();
	}
    }

    /** 
     * Is the constant a known constant? 
     * @return true of the object represents an integer, floating
     * point or string
     */
    public boolean known() {
	return false;
    }

    /**
     * Does this object represent an actual constant?
     * @return true if the object represents a constant
     */
    public boolean constant() {
	return false;
    }
}
