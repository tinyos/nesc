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

; Utility functions
(defun ins (&rest args)
  (insert (apply #'format args)))

(setq basename (car command-line-args-left))
(setq nodedefs (cadr command-line-args-left))
(setq command-line-args-left (cddr command-line-args-left))

(defmacro deffield (field-name c-type attributes)
  `(deffield* ',field-name ',c-type ',attributes))

(defmacro deftype (type-name super-type fields documentation)
  `(deftype* ',type-name ',super-type ',fields ',documentation))

(defmacro defnode (node-name type-name documentation)
  `(defnode* ',node-name ',type-name ',documentation))



(setq fields nil)
(setq types nil)
(setq nodes nil)

(defun deffield* (field-name c-type attributes)
  (setq attributes (attributes-ok field-name attributes))
  (if (assoc field-name fields)
      (message (format "Field %s already defined" field-name))
    (setq fields (cons (list field-name c-type attributes) fields))))

(defun deftype* (type-name super-type fields documentation)
    (if (or (assoc type-name types) (assoc type-name nodes))
      (message (format "Name %s already used for a type or node" type-name))
    (setq types (cons (list type-name super-type fields documentation) types))))


(defun defnode* (node-name type-name documentation)
    (if (or (assoc node-name types) (assoc node-name nodes))
      (message (format "Name %s already used for a type or node" node-name))
    (setq nodes (cons (list node-name type-name documentation) nodes))))


(setq legal-attributes '(init tree nodump noprint default dump-special print-special format))

(defun attributes-ok (field-name attrs)
  (mapcar '(lambda (attr)
	     (let* ((realattr (if (listp attr) attr (list attr)))
		    (aname (car realattr)))
	       (if (not (member aname legal-attributes))
		   (message (format "Unknown attribute %s in field %s"
				  aname field-name)))
	       realattr)) attrs))

(defun check-defs ()
  (setq types (reverse types))
  (setq nodes (reverse nodes))
  (check-types)
  (check-nodes))

(defun check-types ()
  (mapcar #'check-type types))

(defun check-type (type)
  (mapcar '(lambda (field-name)
	     (if (not (assoc field-name fields))
		 (message (format "Unknown field %s in %s" field-name (car type)))))
	  (type-fields type))
  (if (and (type-super-type type)
	   (not (assoc (type-super-type type) types)))
      (message (format "Unknown super-type %s in %s"
		       (type-super-type type) (type-name type)))))


(defun check-nodes ()
  (mapcar #'check-node nodes))

(defun check-node (node)
  (if (not (assoc (node-type node) types))
      (message (format "Unknown type %s in node %s"
		       (node-type node) (node-name node)))))

(defun build-file (name)
  (setq name (concat basename "_" name))
  (let ((buffer (create-file-buffer name))
	(debug-on-error (or debug-on-error
			    (interactive-p))))
    (if debug-on-error
	(switch-to-buffer buffer)
      (set-buffer buffer))
    (fill-buffer)
    (write-file name)
    (unless debug-on-error
      (kill-buffer buffer))))


(require 'cl)

(defun nodes-of (typename)
  (reduce #'(lambda (l x)
	      (if (equal (node-type x) typename)
		  (cons (node-name x) l)
		l))
	  nodes :initial-value nil))

(defun all-type-fields (type)
  (if (type-super-type type)
      (append (all-type-fields (assoc (type-super-type type) types))
	      (type-fields type))
    (type-fields type)))

(defun caddr (x) (car (cddr x)))
(defun cadddr (x) (car (cddr (cdr x))))

(defalias 'type-name #'car)
(defalias 'type-super-type #'cadr)
(defalias 'type-fields #'caddr)
(defalias 'type-documentation #'cadddr)

(defalias 'field-name #'car)
(defalias 'field-c-type #'cadr)
(defalias 'field-attributes #'caddr)

(defalias 'node-name #'car)
(defalias 'node-type #'cadr)
(defalias 'node-documentation #'caddr)

(defun copyright-notice ()
  (insert "/* Automatically generated from " nodedefs ", do not edit. */\n\n")
  (insert "/* See the copyright notice in " nodedefs " */\n"))

;; go ahead and check defs
(load-file nodedefs)
(check-defs)
