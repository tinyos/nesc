; This file is part of the nesC compiler.
; 
; This file is derived from the RC Compiler. It is thus
;    Copyright (C) 2000-2001 The Regents of the University of California.
; Changes for nesC are
;    Copyright (C) 2002 Intel Corporation
; 
; The attached "nesC" software is provided to you under the terms and
; conditions of the GNU General Public License Version 2 as published by the
; Free Software Foundation.
; 
; nesC is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
; 
; You should have received a copy of the GNU General Public License
; along with nesC; see the file COPYING.  If not, write to
; the Free Software Foundation, 59 Temple Place - Suite 330,
; Boston, MA 02111-1307, USA.

(load-file "build-basics.el")

(setq kind_type (concat basename "_kind"))

(setq all-type-names (append (mapcar #'type-name types)
			     (mapcar #'node-name nodes)))
(setq parent-types
      (append
       (mapcar #'(lambda (type) (cons (type-name type) (type-super-type type)))
	       types)
       (mapcar #'(lambda (node) (cons (node-name node) (node-type node)))
	       nodes)))

(defun parentof (name)
  (cdr (assoc name parent-types)))

(setq dfsnumbering nil)

; This is not winning any complexity prizes
(defun dfsnumber (name nextid)
  (let ((myid nextid))
    (mapc #'(lambda (child)
	      (if (equal (parentof child) name)
		  (setq nextid (dfsnumber child (1+ nextid)))))
	  all-type-names)
    (setq dfsnumbering (acons name (cons myid nextid) dfsnumbering))
    nextid))

(dfsnumber 'node 42)

(sort dfsnumbering #'(lambda (entry1 entry2) (< (cadr entry1) (cadr entry2))))

(defun fill-buffer ()
  (copyright-notice)
  (mapc #'write-typedefs types)
  (mapc #'write-node-typedefs nodes)
  (insert "typedef enum {\n");
  (mapc #'write-kinds all-type-names)
  (backward-delete-char 2)
  (insert (format "\n} %s;\n" kind_type))
  (insert (format "\nextern %s %s_parent_kind[]; /* indexed by kind - kind_node */\n"
		  kind_type basename))
  (insert (format "\nextern %s %s_post_kind[]; /* indexed by kind - kind_node */\n"
		  kind_type basename))
  (insert (format "\nextern size_t %s_sizeof[]; /* indexed by kind - kind_node */\n"
		  basename))
  (insert (format "\nextern type_t %s_typeof[]; /* indexed by kind - kind_node */\n"
		  basename))
  (mapc #'write-is-test all-type-names)
  (mapc #'write-reverse all-type-names)
)

(defun write-typedefs (type)
  (insert (format "typedef struct %s_%s *%s;\n"
		  basename (type-name type) (type-name type))))

(defun write-node-typedefs (node)
  (insert (format "typedef struct %s_%s *%s;\n"
		  basename (node-type node) (node-name node))))

(defun write-kinds (name)
  (insert (format "  kind_%s = %s,\n" name (cadr (assoc name dfsnumbering))))
  (insert (format "  postkind_%s = %s,\n" name (cddr (assoc name dfsnumbering)))))

; We should use this one, but I don't feel like changing every is_xxx just now.
(defun write-is-test2 (name)
  (insert (format "#define IS_%s(x) ((x)->kind >= kind_%s && (x)->kind <= postkind_%s)\n"
		  (upcase (format "%s" name)) name name)))

(defun write-is-test (name)
  (insert (format "#define is_%s(x) ((x)->kind >= kind_%s && (x)->kind <= postkind_%s)\n"
		  name name name)))

(defun write-reverse (name)
  (insert (format "#define %s_reverse(x) CAST(%s, ast_reverse(CAST(node, (x))))\n"
		  name name)))

(build-file "types.h")



(defun fill-buffer ()
  (copyright-notice)
  (mapc #'(lambda (type) (write-creator (type-name type) type)) types)
  (mapc #'(lambda (node) (write-creator (node-name node) (assoc (node-type node) types))) nodes)
  (insert "\n\n")
  (mapc #'write-type types)
  (mapc #'write-list all-type-names))

(defun write-type (type)
  (insert (format "/* %s */\n" (type-documentation type)))
  (insert (format "struct %s_%s { /* extends %s */\n"
		  basename (type-name type) (type-super-type type)))
  (write-fields type)
  (insert "};\n\n"))

(defun write-fields (type)
  (if (type-super-type type)
      (write-fields (assoc (type-super-type type) types))
    (insert (format "  %s kind;\n" kind_type)))
  (mapc '(lambda (field-name)
	   (let ((field (assoc field-name fields)))
	     (insert "  "
	      (if (assoc 'format (field-attributes field))
		  (format (field-c-type field) field-name)
		(format "%s %s%s" (field-c-type field)
			(if (assoc 'tree (field-attributes field))
			    "sameregion " "")
			field-name))
	      ";\n")))
	  (type-fields type)))

(defun write-creator (name type)
  (write-creator-header name type)
  (insert ";\n"))

(defun write-creator-header (name type)
  (insert (format "%s new_%s(region r" name name))
  (let ((write-creator-fields
	 #'(lambda (type)
	     (if (type-super-type type)
		 (funcall write-creator-fields
			  (assoc (type-super-type type) types)))
	     (mapcar #'(lambda (field-name)
			 (let ((field (assoc field-name fields)))
			   (if (assoc 'init (field-attributes field))
			       (insert (format ", %s %s"
					       (field-c-type field)
					       field-name)))))
		       (type-fields type)))))
      (funcall write-creator-fields type))
  (insert ")"))

(defun write-list (name)
  (insert (format "%s %s_chain(%s l1, %s l2);\n" name name name name))
  (insert (format "#define scan_%s(var, list) for (var = (list); var; var = CAST(%s, var->next))\n" name name)))

(build-file "defs.h")

(defun fill-buffer ()
  (copyright-notice)
  (mapc #'(lambda (type) (write-creator-source (type-name type) type)) types)
  (mapc #'(lambda (node) (write-creator-source (node-name node) (assoc (node-type node) types))) nodes)
  (insert "\n\n")
  (mapc #'write-list-source all-type-names)
  (write-parent-kinds)
  (write-post-kinds)
  (write-sizes)
  (write-types))

(defun write-creator-source (name type)
  (write-creator-header name type)
  (insert "\n{\n")
  (insert (format "  %s obj = ralloc(r, struct %s_%s);\n\n"
		  name basename (type-name type)))
  (insert (format "  obj->kind = kind_%s;\n" name))
  (let ((write-creator-fields
	 #'(lambda (type)
	     (if (type-super-type type)
		 (funcall write-creator-fields
			  (assoc (type-super-type type) types)))
	     (mapcar #'(lambda (field-name)
			 (let ((field (assoc field-name fields)))
			   (cond ((assoc 'init (field-attributes field))
				  (insert (format "  obj->%s = %s;\n" field-name field-name)))
				 ((assoc 'default (field-attributes field))
				  (insert (format "  obj->%s = %s;\n"
						  field-name
						  (cadr (assoc 'default (field-attributes field)))))))))
		     (type-fields type)))))
    (funcall write-creator-fields type))
  (insert "\n  return obj;\n}\n\n"))

  

(defun write-list-source (name)
  (insert (format "%s %s_chain(%s l1, %s l2)\n" name name name name))
  (insert (format "{ return CAST(%s, %s_chain(CAST(node, l1), CAST(node, l2))); }\n\n" name basename)))

(defun write-parent-kinds ()
  (insert (format "%s %s_parent_kind[] = {\n" kind_type basename))
  (mapc #'(lambda (dfs_entry)
	    (let ((parent (parentof (car dfs_entry))))
	      (if parent
		  (insert (format "  kind_%s,\n" parent))
		(insert "  0,\n"))))
	dfsnumbering)
  (insert "};\n\n"))

(defun write-post-kinds ()
  (insert (format "%s %s_post_kind[] = {\n" kind_type basename))
  (mapc #'(lambda (dfs_entry)
	    (insert (format "  postkind_%s,\n" (car dfs_entry))))
	dfsnumbering)
  (insert "};\n\n"))

(defun struct-name (name)
  (let ((node-entry (assoc name nodes)))
    (if node-entry (node-type node-entry)
      name)))

(defun write-sizes ()
  (insert (format "size_t %s_sizeof[] = {\n" basename))
  (mapc #'(lambda (dfs_entry)
	    (insert (format "  sizeof(struct %s_%s),\n"
			    basename (struct-name (car dfs_entry)))))
	dfsnumbering)
  (insert "};\n\n"))

(defun write-types ()
  (insert (format "type_t %s_typeof[] = {\n" basename))
  (mapc #'(lambda (dfs_entry)
	    (insert (format "  rctypeof(struct %s_%s),\n"
			    basename (struct-name (car dfs_entry)))))
	dfsnumbering)
  (insert "};\n\n"))

(build-file "defs.c")

