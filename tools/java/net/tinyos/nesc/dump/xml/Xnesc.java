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

import org.xml.sax.*;

import java.util.*;

/**
 * Top-level XML dump element. Doesn't do anything useful, see the elements
 * which can be embedded here to actually get at the dumped information.
 * @see net.tinyos.nesc.dump.xml.Xwiring
 * @see net.tinyos.nesc.dump.xml.Xcomponents
 * @see net.tinyos.nesc.dump.xml.Xinterfaces
 * @see net.tinyos.nesc.dump.xml.Xinterfacedefs
 * @see net.tinyos.nesc.dump.xml.Xtags
 *
 * @author contributor: Elaine Cheong <celaine@cvs.sourceforge.net>
 */
public class Xnesc extends NDElement
{
    public static LinkedList/*Xinterface*/ interfaceList;
    public static LinkedList/*Xcomponent*/ componentList;
    public static LinkedList/*Xconstant*/ constantList;
    public static LinkedList/*Xfunction*/ functionList;
    public static LinkedList/*Xinterfacedef*/ interfacedefList;
    public static LinkedList/*TagDefinition*/ tagList;
    public static LinkedList/*Xtypedef*/ typedefList;
    public static LinkedList/*Xvariable*/ variableList;

    public static DefinitionTable defsDataDefinition = new DefinitionTable();
    public static DefinitionTable defsNescDefinition = new DefinitionTable();
    public static DefinitionTable defsTagDefinition = new DefinitionTable();
    public static DefinitionTable defsXfield = new DefinitionTable();
    
    public static void reset() {
        interfaceList = null;
        componentList = null;
        constantList = null;
        functionList = null;
        interfacedefList = null;
        tagList = null;
        typedefList = null;
        variableList = null;

        defsDataDefinition = new DefinitionTable();
        defsNescDefinition = new DefinitionTable();
        defsTagDefinition = new DefinitionTable();
        defsXfield = new DefinitionTable();
    }

    /** Flag for determining whether to call addNewAttributes()
     *  @see net.tinyos.nesc.dump.xml.Xinterface
     *  @see net.tinyos.nesc.dump.xml.DataDefinition
     *  @see net.tinyos.nesc.dump.xml.Definition
     */
    public static boolean addNewAttributes = true;
}
