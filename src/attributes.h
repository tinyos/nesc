#ifndef ATTRIBUTES_H
#define ATTRIBUTES_H

/* Provide warnings about ignored attributes and attribute lists */

void ignored_attribute(attribute attr);
void ignored_attributes(attribute alist);
void ignored_dd_attributes(dd_list alist);
void ignored_gcc_attribute(gcc_attribute attr);
void ignored_nesc_attribute(nesc_attribute attr);

/* handle_X_attribute(attr, obj):
   Attempt to apply attribute attr to obj of kind X (decl, field, tag, type),
   modifying obj.

   For decls, fields, tags:
     If attr is not applicable: issue a warning with ignored_attributes
   For types:
     Return TRUE if applicable, FALSE if not
   (this difference is due to the funky rules of attributes used as type qualifiers)
*/

void handle_nescdecl_attribute(attribute attr, nesc_declaration ndecl);
void handle_decl_attribute(attribute attr, data_declaration ddecl);
void handle_field_attribute(attribute attr, field_declaration fdecl);
void handle_tag_attribute(attribute attr, tag_declaration tdecl);
bool handle_type_attribute(attribute attr, type *t);

/* Functions to handle regular and dd list of attributes */
void handle_nescdecl_attributes(attribute alist, nesc_declaration ndecl);
void handle_decl_attributes(attribute alist, data_declaration ddecl);
void handle_field_attributes(attribute alist, field_declaration fdecl);
void handle_tag_attributes(attribute alist, tag_declaration tdecl);
void handle_nescdecl_dd_attributes(dd_list alist, nesc_declaration ndecl);
void handle_decl_dd_attributes(dd_list alist, data_declaration ddecl);
void handle_field_dd_attributes(dd_list alist, field_declaration fdecl);
void handle_tag_dd_attributes(dd_list alist, tag_declaration tdecl);

#endif
