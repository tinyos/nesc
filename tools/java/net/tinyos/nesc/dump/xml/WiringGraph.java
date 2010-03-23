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

import java.util.*;

/**
 * A nesC wiring graph. A slightly unconventional graph where the nodes
 * (WiringNode) may represent one or more actual graph nodes (in the case
 * where the node corresponds to a parameterised function or interface).
 * <p>
 * A position in this graph is represented by an "endpoint", which is a
 * WiringNode plus optional arguments (the optional arguments identify a
 * particular interface or function on nodes representing parameterised
 * functions or interfaces). Note that an endpoint may itself represent
 * multiple nodes in the case where the node is parameterised and the
 * endpoint has no arguments.
 * <p>
 * Edges in this graphs (Xwire) connect two endpoints.
 *
 * @see net.tinyos.nesc.dump.xml.WiringNode
 * @see net.tinyos.nesc.dump.xml.Xwire
 * @see net.tinyos.nesc.dump.xml.WiringEndpoint
 */
public class WiringGraph
{
    protected Hashtable endpoints = new Hashtable();

    /**
     * Find the node for a particular definition. Adds a new node if 
     * the definition is not found.
     * @param epDecl Node to lookup.
     * @return WiringNode for epDecl.
     */
    public WiringNode lookup(DataDefinition epDecl) {
	WiringNode found = (WiringNode)endpoints.get(epDecl);

	if (found == null) {
	    found = new WiringNode(epDecl);
	    endpoints.put(epDecl, found);
	}
	return found;
    }

    /**
     * Add edge to the graph.
     * @param wire Edge to add.
     */
    public void addEdge(Xwire wire) {
	wire.from.node = lookup(wire.from.node.ep);
	wire.from.node.addToEdge(wire);

	wire.to.node = lookup(wire.to.node.ep);
	wire.to.node.addFromEdge(wire);
    }
}
