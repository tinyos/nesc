;;; necs.el --- nesC mode

;; Copyright (C) 1985-2002 by Free Software Foundation, Inc.
;; Copyright (C) 2004 Intel Corporation

;; Author: Dennis Haney <davh@diku.dk>
;;         David Gay <dgay@intel-research.net>
;; Maintainer: David Gay <dgay@intel-research.net>
;; Keywords: c, languages

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License Version 2
;; as published by the Free Software Foundation.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;

;;; Code:

(require 'cc-mode)

(if (not (string-match "^5.2[0-9]" c-version))
    (error "cc-mode 5.30 and later not supported by this file"))

(require 'font-lock)

(defconst nesc-keywords
  (eval-when-compile
    (regexp-opt
     '("abstract" "as" "atomic" "async"
       "call" "command" "components" "configuration" 
       "event" "implementation" "interface" "includes" 
       "module" "norace" "nx_struct" "nx_union" "post" "provides"
       "signal" "task" "uses" ) t)))

(setq nesc-font-lock-keywords-1
      (list
       `(eval .
	      (cons (concat "\\<" (,@ nesc-keywords) "\\>") 'font-lock-keyword-face))))

(defconst nesc-font-lock-keywords
  (append nesc-font-lock-keywords-1
   c++-font-lock-keywords-2))

(defvar nesc-mode-abbrev-table nil
  "Abbreviation table used in nesc-mode buffers.")
(define-abbrev-table 'nesc-mode-abbrev-table
  '(("else" "else" c-electric-continued-statement 0)
    ("while" "while" c-electric-continued-statement 0)))

(defvar nesc-mode-map ()
  "Keymap used in nesc-mode buffers.")
(if nesc-mode-map
    nil
  (setq nesc-mode-map (c-make-inherited-keymap))
  ;; add bindings which are only useful for nesC
  )

(easy-menu-define c-nesc-menu nesc-mode-map "nesC Mode Commands"
		  (c-mode-menu "nesC"))

(defvar nesc-mode-syntax-table nil
  "Syntax table used in nesc-mode buffers.")
(if nesc-mode-syntax-table
    ()
  (setq nesc-mode-syntax-table (make-syntax-table))
  (c-populate-syntax-table nesc-mode-syntax-table))

(defconst c-nesC-comment-start-regexp c-C++-comment-start-regexp)
(defconst c-nesC-class-kwds "nx_struct\\|nx_union\\|struct\\|union\\|implementation")
(defconst c-nesC-class-key (c-paren-re c-nesC-class-kwds))

(defvar cc-imenu-nesc-generic-expression
  cc-imenu-c-generic-expression
  "Imenu generic expression for nesC mode.  See `imenu-generic-expression'.")

(defun nesc-mode ()
  "Major mode for editing nesC code."
  (interactive)
  (c-initialize-cc-mode)
  (kill-all-local-variables)
  (set-syntax-table nesc-mode-syntax-table)
  (setq major-mode 'nesc-mode
 	mode-name "nesC"
	local-abbrev-table nesc-mode-abbrev-table
	abbrev-mode t
	; we have javadoc-style comments
	c-append-paragraph-start c-Java-javadoc-paragraph-start)
  (use-local-map nesc-mode-map)
  (c-common-init)
  (setq comment-start "// "
 	comment-end   ""
        c-keywords (c-identifier-re (concat c-C-keywords "\\|" nesc-keywords))
 	c-conditional-key c-C-conditional-key
 	c-comment-start-regexp c-nesC-comment-start-regexp
  	c-class-key c-nesC-class-key
	c-method-key nil
 	c-baseclass-key nil
	c-recognize-knr-p nil
	c-inexpr-class-key nil
	;defun-prompt-regexp c-nesC-defun-prompt-regexp
	)
  (cc-imenu-init cc-imenu-nesc-generic-expression)
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults 
        '(nesc-font-lock-keywords nil t))
  (run-hooks 'c-mode-common-hook)
  (run-hooks 'nesc-mode-hook)
  (c-update-modeline))

(provide 'nesc-mode)

;;; necs.el ends here
