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
