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

;; Build a function to set the parent and parent_ptr nodes in a tree

(load-file "build-basics.el")

(defun fill-buffer ()
  (copyright-notice)
  (mapcar #'write-parent-type types))

(defun write-parent-type (type)
  (let* ((name (type-name type))
	 (synonyms (nodes-of name)))
    (mapcar #'write-parent-case (cons name synonyms))
    (let ((tree-fields 
	   (remove-if-not
	    #'(lambda (field)
		(assoc 'tree (field-attributes (assoc field fields))))
	    (all-type-fields type))))
      (if tree-fields
	  (progn
	    (insert (format "{\n  %s x = CAST(%s, n);\n\n" name name))
	    (mapcar #'write-parent-field tree-fields)
	    (insert "  break;\n}\n"))
	(insert "break;\n")))))

(defun write-parent-case (name)
  (insert (format "case kind_%s: " name)))

(defun write-parent-field (field)
  (insert (format "  AST_set_parent_list(&x->%s, n);\n" field)))

(build-file "parent.c")
