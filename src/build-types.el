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

(reduce 
 #'(lambda (nextid type)
     (if (eq (type-super-type type) '())
	 (1+ (dfsnumber (type-name type) nextid))
       nextid))
 types :initial-value (string-to-number (car command-line-args-left)))

(setq dfsnumbering (sort dfsnumbering #'(lambda (entry1 entry2) (< (cadr entry1) (cadr entry2)))))

(defun fill-buffer ()
  (copyright-notice)
  (mapc #'write-typedefs types)
  (mapc #'write-node-typedefs nodes)
  (ins "typedef enum {\n")		;
  (mapc #'write-kinds all-type-names)
  (backward-delete-char 2)
  (ins "\n} %s;\n" kind_type)
  (ins "\nextern %s %s_parent_kind[]; /* indexed by kind - kind_node */\n"
       kind_type basename)
  (ins "\nextern %s %s_post_kind[]; /* indexed by kind - kind_node */\n"
       kind_type basename)
  (ins "\nextern size_t %s_sizeof[]; /* indexed by kind - kind_node */\n"
       basename)
  (ins "\nextern type_t %s_typeof[]; /* indexed by kind - kind_node */\n"
       basename)
  (mapc #'write-is-test all-type-names)
  (write-cast))

(defun write-typedefs (type)
  (ins "typedef struct %s_%s *%s;\n"
       basename (type-name type) (type-name type)))

(defun write-node-typedefs (node)
  (ins "typedef struct %s_%s *%s;\n"
       basename (node-type node) (node-name node)))

(defun write-kinds (name)
  (ins "  kind_%s = %s,\n" name (cadr (assoc name dfsnumbering)))
  (ins "  postkind_%s = %s,\n" name (cddr (assoc name dfsnumbering))))

					; We should use this one, but I don't feel like changing every is_xxx just now.
(defun write-is-test2 (name)
  (ins "#define IS_%s(x) ((x)->kind >= kind_%s && (x)->kind <= postkind_%s)\n"
       (upcase (format "%s" name)) name name))

(defun write-is-test (name)
  (ins "#define is_%s(x) ((x)->kind >= kind_%s && (x)->kind <= postkind_%s)\n"
       name name name))

(defun write-cast ()
  (ins "typedef struct\n")
  (ins "{\n")
  (ins "  AST_kind kind;\n")
  (ins "} *%s_generic;\n\n" basename)
  (ins "#ifdef __GNUC__\n")
  (ins "#define %s_CAST(type, x) ({%s_generic tEmPcast = (%s_generic)(x); if (tEmPcast) assert(is_ ## type(tEmPcast)); (type)(tEmPcast); })\n" basename basename basename)
  (ins "#define %s_CASTPTR(type, x) ({%s_generic *tEmPcast = (%s_generic *)(x); if (tEmPcast && *tEmPcast) assert(is_ ## type(*tEmPcast)); (type *)(tEmPcast); })\n" basename basename basename)
  (ins "#define %s_CASTSRPTR(type, x) ({%s_generic *tEmPcast = (%s_generic *)(x); if (tEmPcast && *tEmPcast) assert(is_ ## type(*tEmPcast)); (type sameregion *)(tEmPcast); })\n" basename basename basename)
  (ins "#else\n")
  (ins "/* Could also generate some code to make this safe */\n")
  (ins "#define %s_CAST(type, x) ((type)(x))\n" basename)
  (ins "#define %s_CASTPTR(type, x) ((type *)(x))\n" basename)
  (ins "#define %s_CASTSRPTR(type, x) ((type sameregion *)(x))\n" basename)
  (ins "#endif\n"))  

(build-file "types.h")

(defun fill-buffer ()
  (copyright-notice)
  (mapc #'write-type types)
  (ins "\n\n")
  (mapc #'(lambda (type) (write-creator (type-name type) type)) types)
  (mapc #'(lambda (node) (write-creator (node-name node) (assoc (node-type node) types))) nodes))

(defun write-type (type)
  (ins "/* %s */\n" (type-documentation type))
  (ins "struct %s_%s { /* extends %s */\n"
       basename (type-name type) (type-super-type type))
  (write-fields type)
  (ins "};\n\n"))

(defun write-fields (type)
  (if (type-super-type type)
      (write-fields (assoc (type-super-type type) types))
    (ins "  %s kind;\n" kind_type))
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
  (ins ";\n"))

(defun write-creator-header (name type)
  (ins "%s new_%s(region r" name name)
  (let ((write-creator-fields
	 #'(lambda (type)
	     (if (type-super-type type)
		 (funcall write-creator-fields
			  (assoc (type-super-type type) types)))
	     (mapcar #'(lambda (field-name)
			 (let ((field (assoc field-name fields)))
			   (if (assoc 'init (field-attributes field))
			       (ins ", %s %s"
				    (field-c-type field)
				    field-name))))
		     (type-fields type)))))
    (funcall write-creator-fields type))
  (ins ")"))

(build-file "defs.h")

(defun fill-buffer ()
  (copyright-notice)
  (mapc #'(lambda (type) (write-creator-source (type-name type) type)) types)
  (mapc #'(lambda (node) (write-creator-source (node-name node) (assoc (node-type node) types))) nodes)
  (ins "\n\n")
  (write-parent-kinds)
  (write-post-kinds)
  (write-sizes)
  (write-types))

(defun write-creator-source (name type)
  (write-creator-header name type)
  (ins "\n{\n")
  (ins "  %s obj = ralloc(r, struct %s_%s);\n\n"
       name basename (type-name type))
  (ins "  obj->kind = kind_%s;\n" name)
  (let ((write-creator-fields
	 #'(lambda (type)
	     (if (type-super-type type)
		 (funcall write-creator-fields
			  (assoc (type-super-type type) types)))
	     (mapcar #'(lambda (field-name)
			 (let ((field (assoc field-name fields)))
			   (cond ((assoc 'init (field-attributes field))
				  (ins "  obj->%s = %s;\n" field-name field-name))
				 ((assoc 'default (field-attributes field))
				  (ins "  obj->%s = %s;\n"
				       field-name
				       (cadr (assoc 'default (field-attributes field))))))))
		     (type-fields type)))))
    (funcall write-creator-fields type))
  (ins "\n  return obj;\n}\n\n"))

(defun write-parent-kinds ()
  (ins "%s %s_parent_kind[] = {\n" kind_type basename)
  (mapc #'(lambda (dfs_entry)
	    (let ((parent (parentof (car dfs_entry))))
	      (if parent
		  (ins "  kind_%s,\n" parent)
		(ins "  0,\n"))))
	dfsnumbering)
  (ins "};\n\n"))

(defun write-post-kinds ()
  (ins "%s %s_post_kind[] = {\n" kind_type basename)
  (mapc #'(lambda (dfs_entry)
	    (ins "  postkind_%s,\n" (car dfs_entry)))
	dfsnumbering)
  (ins "};\n\n"))

(defun struct-name (name)
  (let ((node-entry (assoc name nodes)))
    (if node-entry (node-type node-entry)
      name)))

(defun write-sizes ()
  (ins "size_t %s_sizeof[] = {\n" basename)
  (mapc #'(lambda (dfs_entry)
	    (ins "  sizeof(struct %s_%s),\n"
		 basename (struct-name (car dfs_entry))))
	dfsnumbering)
  (ins "};\n\n"))

(defun write-types ()
  (ins "type_t %s_typeof[] = {\n" basename)
  (mapc #'(lambda (dfs_entry)
	    (ins "  rctypeof(struct %s_%s),\n"
		 basename (struct-name (car dfs_entry))))
	dfsnumbering)
  (ins "};\n\n"))

(build-file "types.c")

