/* This file is part of the nesC compiler.
   Copyright (C) 2002 Intel Corporation

The attached "nesC" software is provided to you under the terms and
conditions of the GNU General Public License Version 2 as published by the
Free Software Foundation.

nesC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with nesC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#ifndef NESC_COMPONENT_H

void build_component(region r, nesc_declaration cdecl);

void declare_interface_ref(interface_ref iref, declaration gparms,
			   environment genv, attribute attribs);

void make_implicit_interface(data_declaration fndecl,
			     function_declarator fdeclarator);

void check_interface_parameter_types(declaration parms);

environment start_implementation(void);

void interface_scan(data_declaration iref, env_scanner *scan);
data_declaration interface_lookup(data_declaration iref, const char *name);

void component_spec_iterate(nesc_declaration c,
			    void (*iterator)(data_declaration fndecl,
					     void *data),
			    void *data,
			    bool interfaces,
			    bool otherdecls);

void component_functions_iterate(nesc_declaration c,
				 void (*iterator)(data_declaration fndecl,
						  void *data),
				 void *data);

nesc_declaration specification_copy(region r, component_ref cref,
				    bool copy_is_abstract);
/* Effects: Make a "shallow" copy of component specified by `cref' in region r,
     i.e., identical to cref->cdecl except that it has a copy of the
     specification (including a copy of each interface instance)
     The copy's instance_name is set to the name specified in cref (word2)
   Returns: The shallow copy
*/

void build_external_graph(region r, nesc_declaration cdecl);

void copy_interface_functions(region r, nesc_declaration container,
			      data_declaration iref, environment fns);


extern bool generic_used;	/* Hack to prevent doc generation until
				   new doc system built */

#endif
