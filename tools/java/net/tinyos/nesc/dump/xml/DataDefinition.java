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

/*
@Copyright (c) 2005 The Regents of the University of California.
All rights reserved.

Permission is hereby granted, without written agreement and without
license or royalty fees, to use, copy, modify, and distribute this
software and its documentation for any purpose, provided that the
above copyright notice and the following two paragraphs appear in all
copies of this software.

IN NO EVENT SHALL THE UNIVERSITY OF CALIFORNIA BE LIABLE TO ANY PARTY
FOR DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES
ARISING OUT OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN IF
THE UNIVERSITY OF CALIFORNIA HAS BEEN ADVISED OF THE POSSIBILITY OF
SUCH DAMAGE.

THE UNIVERSITY OF CALIFORNIA SPECIFICALLY DISCLAIMS ANY WARRANTIES,
INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE. THE SOFTWARE
PROVIDED HEREUNDER IS ON AN "AS IS" BASIS, AND THE UNIVERSITY OF
CALIFORNIA HAS NO OBLIGATION TO PROVIDE MAINTENANCE, SUPPORT, UPDATES,
ENHANCEMENTS, OR MODIFICATIONS.

                                                PT_COPYRIGHT_VERSION_2
                                                COPYRIGHTENDKEY


*/

package net.tinyos.nesc.dump.xml;

import net.tinyos.nesc.dump.*;
import org.xml.sax.*;
import java.util.*;

/**
 * Base class for definitions of C objects (typedefs, variables, functions, 
 * constants, interfaces, and internal-components (component references in
 * configurations). These are uniquely identified by their 'ref' attribute.
 *
 * @author contributor: Elaine Cheong <celaine@cvs.sourceforge.net>
 */
abstract public class DataDefinition extends CDefinition
{
    /**
     * Name of this object. Not globally unique.
     */
    public String name; 
    /**
     * Unique identifier for this object.
     */
    public String ref;

    /**
     * (definition only) Type of this object.
     */
    public Type type;

    /* for reference handling */
    public void init(Attributes attrs) {
	super.init(attrs);
	ref = attrs.getValue("ref");
	name = attrs.getValue("name");
	/* ignoring scoped for now */
    }

    /* for adding new attributes (does not overwrite existing ones) */
    public void addNewAttributes(Attributes attrs) {
        super.addNewAttributes(attrs);
	if (ref == null) {
            ref = attrs.getValue("ref");
        }
	if (name == null) {
            name = attrs.getValue("name");
        }
	/* ignoring scoped for now */
    }
    
    public synchronized NDElement start(Attributes attrs) {
	return Xnesc.defsDataDefinition.define(attrs.getValue("ref"), attrs, this);
    }

    static synchronized Definition lookup(NDReader reader, Attributes attrs, 
					  String elementName) {
	return Xnesc.defsDataDefinition.lookup(reader, attrs.getValue("ref"), attrs, elementName);
    }

    public void child(NDElement subElement) {
	super.child(subElement);
	if (subElement instanceof Type)
	    type = (Type)subElement;
    }

    public String toString() {
	if (container == null)
	    return name;
	return container.toString() + "." + name;
    }

    public String debugString() {
	String base = "";
	//base += "[" + super.toString() + "]";
	if (name != null)
	    return base + "C(" + name + ", " + ref + ")";
	else
	    return  base + "C(" + ref + ")";
    }
}
