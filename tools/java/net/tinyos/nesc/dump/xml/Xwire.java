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
 * An edge in a wiring graph.
 * @see net.tinyos.nesc.dump.xml.WiringGraph
 * @see net.tinyos.nesc.dump.xml.WiringEndpoint
 * @see net.tinyos.nesc.dump.xml.WiringNode
 */
public class Xwire extends NDElement
{
    /**
     * The origin of this wire.
     */
    public WiringEndpoint from;

    /**
     * The destination of this wire.
     */
    public WiringEndpoint to;

    /**
     * The source code location which created this wire. null in
     * function-level wiring graphs.
     */
    public Location location;

    public NDElement start(Attributes attrs) {
	location = Location.decode(attrs.getValue("loc"));
	return this;
    }

    public void child(NDElement subElement) {
	super.child(subElement);
	if (subElement instanceof Xfrom)
	    from = (Xfrom)subElement;
	if (subElement instanceof Xto)
	    to = (Xto)subElement;
    }

    protected boolean follow(WiringEndpoint position,
			     WiringEndpoint start, WiringEndpoint end) {
	/* assert(position.node == start.node); */
	if (position.arguments != null) {
	    if (start.arguments != null) {
		if (!start.arguments.equals(position.arguments))
		    return false;
		position.arguments = end.arguments;
	    }
	    /* else assert(end.arguments == null); */
	}
	else
	    position.arguments = end.arguments;
	position.node = end.node;
	return true;
    }


    /**
     * Try to follow this edge forwards from 'position'. Return true if
     * this edge can be followed. If the edge can be followed, update
     * 'position' to reflect the results of following the edge.
     * <p>
     * The position's node must match this edge's starting node ('from' 
     * field).
     * <p>
     * An edge cannot be followed if 'position' specifies a particular
     * interface from a parameterised interface (i.e., has arguments),
     * and this edge specifies a different interface (i.e., has different
     * arguments in its 'from' endpoint).
     */
    public boolean followForwards(WiringEndpoint position) {
	return follow(position, from, to);
    }

    /**
     * Try to follow this edge backwards from 'position'. Return true if
     * this edge can be followed. If the edge can be followed, update
     * 'position' to reflect the results of following the edge.
     * <p>
     * The position's node must match this edge's ending node ('to' field).
     * <p>
     * An edge cannot be followed if 'position' specifies a particular
     * interface from a parameterised interface (i.e., has arguments),
     * and this edge specifies a different interface (i.e., has different
     * arguments in its 'to' endpoint).
     */
    public boolean followBackwards(WiringEndpoint position) {
	return follow(position, to, from);
    }
}
