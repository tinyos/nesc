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

#ifndef NESC_ABSTRACT_H
#define NESC_ABSTRACT_H

void instantiate(nesc_declaration component, expression arglist);
/* Effects: Actually instantiate an abstract component.
     For modules: temp noop
     For configurations: make new shallow copies of included abstract
       components, and copy connection graph (using the new shallow
       copies) 
*/

void push_instance(nesc_declaration component);
/* Effects: push (concrete) component on the stack and set its full instance
     name.
*/

nesc_declaration abstract_recursion(void);
/* Returns:  If the instance stack indicates the programmer has
     created an instantiation loop, i.e., component Y (instance of
     abstract component X) has caused the instantiation of the top-most
     component (another instance of X).
     Return Y if this is the case, NULL if not.
*/

void fold_program(nesc_declaration program, nesc_declaration scheduler);

void pop_instance(void);

void init_abstract(void);

void check_abstract_arguments(const char *kind, data_declaration ddecl,
			      declaration parms, expression arglist);

nesc_declaration interface_copy(region r, interface_ref iref,
				bool copy_is_abstract);
/* Returns: A copy of abstract interface intf, instantiated with arguments
     in arglist.
*/

nesc_declaration specification_copy(region r, component_ref cref,
				    bool copy_is_abstract);
/* Returns: A copy of the parameters and specification of the
     component specified by cref, with arguments specified by cref
*/

#endif
