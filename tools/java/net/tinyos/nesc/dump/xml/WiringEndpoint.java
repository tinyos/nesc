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

import org.xml.sax.*;

/**
 * A position in a wiring graph, i.e., a node and an optional argument list.
 *
 * @see net.tinyos.nesc.dump.xml.WiringGraph
 * @see net.tinyos.nesc.dump.xml.WiringNode
 * @see net.tinyos.nesc.dump.xml.Xwire
 */
public class WiringEndpoint extends NDElement
{
    /**
     * The graph node of this endpoint.
     */
    public WiringNode node;

    /**
     * Arguments to the graph node for this endpoint. May be null.
     */
    public Xarguments arguments;

    public void child(NDElement subElement) {
	if (subElement instanceof DataDefinition)
	    node = new WiringNode((DataDefinition)subElement);
	if (subElement instanceof Xarguments)
	    arguments = (Xarguments)subElement;
    }
    
    /**
     * Create a new, unitialised wiring endpoint.
     */
    public WiringEndpoint() { }

    /**
     * Create a new wiring endpoint on wiring graph node n.
     * @param n Wiring node graph to create endpoint for.
     */
    public WiringEndpoint(WiringNode n) { 
	node = n;
    }

    /**
     * Create a new wiring endpoint on wiring graph node n, with arguments a.
     * @param n Wiring node graph to create endpoint for.
     * @param a Arguments for endpoint.
     */
    public WiringEndpoint(WiringNode n, Xarguments a) { 
	node = n;
	arguments = a;
    }

    /**
     * Create a new wiring endpoint as a copy of endpoint p.
     * @param p endpoint to copy.
     */
    public WiringEndpoint(WiringEndpoint p) { 
	copy(p);
    }
    
    /**
     * Copy endpoint from into this endpoint.
     * @param from endpoint to copy.
     */
    public void copy(WiringEndpoint from) {
	node = from.node;
	arguments = from.arguments;
    }

    public String toString() {
	return node.toString();
    }
}
