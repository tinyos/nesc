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

(setq list-base-type (car command-line-args-left))

(setq all-type-names (append (mapcar #'type-name types)
			     (mapcar #'node-name nodes)))
(defun fill-buffer ()
  (copyright-notice)
  (write-base-function-headers)
  (mapc #'write-macros all-type-names)
  (mapc #'write-function-headers all-type-names)
  )

(defun write-base1 (fname result)
  (ins "%s %s_%s_%s(%s l)"
       result basename list-base-type fname
       list-base-type))

(defun write-base2 (fname result)
  (ins "%s %s_%s_%s(%s l1, %s l2)"
       result basename list-base-type fname
       list-base-type list-base-type))

(defun write-base-function-headers ()
  (write-base1 "last" list-base-type) (ins ";\n")
  (write-base2 "chain" list-base-type) (ins ";\n")
  (write-base1 "length" "int") (ins ";\n")
  (write-base1 "reverse" list-base-type) (ins ";\n"))

(defun write-macros (name)
  (ins "#define %s_reverse(x) CAST(%s, %s_%s_reverse(CAST(%s, (x))))\n"
       name name basename list-base-type list-base-type)
  (ins "#define %s_length(x) %s_%s_length(CAST(%s, (x)))\n"
       name basename list-base-type list-base-type)
  (ins "#define last_%s(x) CAST(%s, %s_%s_last(CAST(%s, (x))))\n"
       name name basename list-base-type list-base-type)
  (ins "#define scan_%s(var, list) for (var = (list); var; var = CAST(%s, var->next))\n" name name))

(defun write-function-headers (name)
  (ins "%s %s_chain(%s l1, %s l2);\n" name name name name))

(build-file (format "list_%s.h" list-base-type))

(defun fill-buffer ()
  (copyright-notice)
  (write-base-function-source)
  (mapc #'write-function-source all-type-names))

(defun write-base-function-source ()
  (write-base1 "last" list-base-type)
  (ins "{\n")
  (ins "  if (!l) return NULL;\n")
  (ins "  while (l->next) l = l->next;\n")
  (ins "  return l;\n")
  (ins "}\n\n")
  
  (write-base2 "chain" list-base-type)
  (ins "{\n")
  (ins "  if (!l1) return l2;\n")
  (ins "  %s_%s_last(l1)->next = l2;\n" basename list-base-type)
  (ins "  return l1;\n")
  (ins "}\n\n")

  (write-base1 "length" "int")
  (ins "{\n")
  (ins "  int len = 0;\n\n")
  (ins "  while (l) \n")
  (ins "    {\n")
  (ins "      l = l->next;\n")
  (ins "      len++;\n")
  (ins "    }\n")
  (ins "  return len;\n")
  (ins "}\n\n")

  (write-base1 "reverse" list-base-type)
  (ins "{\n")
  (ins "  %s last = NULL, next;\n\n" list-base-type)
  (ins "  for (;;)\n")
  (ins "    {\n")
  (ins "      if (!l)\n")
  (ins "        return last;\n")
  (ins "      next = l->next;\n")
  (ins "      l->next = last;\n")
  (ins "      last = l;\n")
  (ins "      l = next;\n")
  (ins "    }\n")
  (ins "}\n\n"))

(defun write-function-source (name)
  (ins "%s %s_chain(%s l1, %s l2)\n" name name name name)
  (ins "{ return CAST(%s, %s_%s_chain(CAST(%s, l1), CAST(%s, l2))); }\n\n" name basename list-base-type list-base-type list-base-type))

(build-file (format "list_%s.c" list-base-type))
