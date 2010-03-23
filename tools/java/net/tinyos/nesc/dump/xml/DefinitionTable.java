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
import java.util.*;

/**
 * Internal class for matching references to definitions.
 */
class DefinitionTable
{
    protected Hashtable allDefinitions = new Hashtable();

    /**
     * Reference element found. Lookup object table to see if the referenced
     * object already exists. If not, create an object of class 'elementName'
     * call it's 'init' method, passing 'attrs' as its arguments, and add it
     * to the object table.
     * @param reader The current NDReader.
     * @param key The key uniquely identifying the nesC object (typically
     *   a string).
     * @param attrs Attributes from the reference element.
     * @param elementName Name of the definition element for this object.
     * @return The Java object for this nesC object.
     */
    synchronized Definition lookup(NDReader reader, Object key,
				   Attributes attrs, String elementName) {
	Definition me = (Definition)allDefinitions.get(key);
	if (me == null) {
	    try {
		me = (Definition)reader.makeElement(elementName);
	    }
	    catch (Exception e) {
		throw new RuntimeException("wrong object class");
	    }
	    me.init(attrs);
	    allDefinitions.put(key, me);
	}
	return me;
    }

    /**
     * Definition element found. Lookup object table to see if the
     * defined object already exists. If found, just return that
     * object. If not, add 'def' as the representative for this nesC
     * object and call 'def.init(attrs)'.  In both cases, set the
     * 'definitionAvailable' for the object to true.
     * @param key The key uniquely identifying the nesC object (typically
     *   a string).
     * @param attrs Attributes from the definition element.
     * @param def Java object created for the definition element.
     * @return The Java object for this nesC object.
     */
    synchronized Definition define(Object key, Attributes attrs, Definition def) {
	Definition me = (Definition)allDefinitions.get(key);
	if (me == null) {
	    me = def;
	    me.init(attrs);
	    allDefinitions.put(key, me);
	}
	me.definitionAvailable = true;
	return me;
    }
}
