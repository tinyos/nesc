;; necs.el --- nesC mode

;; Copyright (C) 2004 Intel Corporation

;; Author:     2002 Martin Stjernholm
;;	       2004 David Gay
;; Maintainer: David Gay <dgay@intel-research.net>
;; Created:    March 2004

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License Version 2
;; as published by the Free Software Foundation.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Code:

(require 'cc-mode)

(if (string-match "^5.2[0-9]" c-version)
    (error "cc-mode 5.30 or later required by this file"))

;; These are only required at compile time to get the sources for the
;; language constants.  (The cc-fonts require and the font-lock
;; related constants could additionally be put inside an
;; (eval-after-load "font-lock" ...) but then some trickery is
;; necessary to get them compiled.)
(eval-when-compile
  (require 'cc-langs)
  (require 'cc-fonts))

(eval-and-compile
  ;; Make our mode known to the language constant system.  Use C
  ;; mode as the fallback for the constants we don't change here.
  ;; This needs to be done also at compile time since the language
  ;; constants are evaluated then.
  (c-add-language 'nesc-mode 'c-mode)
  ;; cc-mode 5.30.8 is buggy:
  (if (not (get 'nesc-mode 'c-fallback-mode))
      (put 'nesc-mode 'c-fallback-mode 'c-mode)))

(c-lang-defconst c-class-decl-kwds
  nesc (append '("interface" "implementation" "nx_struct" "nx_union")
	       (c-lang-const c-class-decl-kwds)))

(c-lang-defconst c-type-prefix-kwds
  nesc (append '("nx_struct" "nx_union")
	       (c-lang-const c-type-prefix-kwds)))

(c-lang-defconst c-block-decls-with-vars
  nesc (append '("nx_struct" "nx_union")
	       (c-lang-const c-block-decls-with-vars)))

(c-lang-defconst c-brace-list-decl-kwds
  nesc (append '("module" "configuration" "provides" "uses")
	       (c-lang-const c-brace-list-decl-kwds)))

(c-lang-defconst c-typeless-decl-kwds
  nesc (append '("as" "components" "interface")
	       (c-lang-const c-typeless-decl-kwds)))

(c-lang-defconst c-modifier-kwds
  nesc (append '("command" "event" "task" "norace" "async")
	       (c-lang-const c-modifier-kwds)))

(c-lang-defconst c-other-decl-kwds
  nesc (append '("includes") (c-lang-const c-other-decl-kwds)))

(c-lang-defconst c-other-kwds
  nesc (append '("new") (c-lang-const c-other-kwds)))

(c-lang-defconst c-block-stmt-1-kwds
  nesc (append '("atomic") (c-lang-const c-block-stmt-1-kwds)))

(c-lang-defconst c-recognize-knr-p nesc nil)

;; This gives post, call, signal a slightly incorrect priority
(c-lang-defconst c-operators
  nesc (append '((prefix "post" "call" "signal"))
	       ;; Note: need to ask specifically for the C operators
	       ;; as there are explicit tests for the C name in the 
	       ;; c-operators constant specification...
	       (c-lang-const c-operators c)))

(c-lang-defconst c-other-kwds
  nesc (cons "abstract" (c-lang-const c-other-kwds)))


(defcustom nesc-font-lock-extra-types '("result_t" "bool"
					"int8_t" "uint8_t"
					"int16_t" "uint16_t"
					"int32_t" "uint32_t")
  "*List of extra types (aside from the type keywords) to recognize in nesC mode.
Each list item should be a regexp matching a single identifier.")

(defconst nesc-font-lock-keywords-1 (c-lang-const c-matchers-1 nesc)
  "Minimal highlighting for nesC mode.")

(defconst nesc-font-lock-keywords-2 (c-lang-const c-matchers-2 nesc)
  "Fast normal highlighting for nesC mode.")

(defconst nesc-font-lock-keywords-3 (c-lang-const c-matchers-3 nesc)
  "Accurate normal highlighting for nesC mode.")

(defvar nesc-font-lock-keywords nesc-font-lock-keywords-3
  "Default expressions to highlight in nesC mode.")

(defvar nesc-mode-syntax-table nil
  "Syntax table used in nesc-mode buffers.")
(or nesc-mode-syntax-table
    (setq nesc-mode-syntax-table
	  (funcall (c-lang-const c-make-mode-syntax-table nesc))))

(defvar nesc-mode-abbrev-table nil
  "Abbreviation table used in nesc-mode buffers.")
(c-define-abbrev-table 'nesc-mode-abbrev-table
  ;; Keywords that if they occur first on a line might alter the
  ;; syntactic context, and which therefore should trig reindentation
  ;; when they are completed.
  '(("else" "else" c-electric-continued-statement 0)
    ("while" "while" c-electric-continued-statement 0)))

(defvar nesc-mode-map (let ((map (c-make-inherited-keymap)))
		      ;; Add bindings which are only useful for nesC
		      map)
  "Keymap used in nesc-mode buffers.")

(easy-menu-define nesc-menu nesc-mode-map "nesC Mode Commands"
		  ;; Can use `nesc' as the language for `c-mode-menu'
		  ;; since its definition covers any language.  In
		  ;; this case the language is used to adapt to the
		  ;; nonexistence of a cpp pass and thus removing some
		  ;; irrelevant menu alternatives.
		  (cons "nesC" (c-lang-const c-mode-menu nesc)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.nc\\'" . nesc-mode))

;;;###autoload
(defun nesc-mode ()
  "Major mode for editing nesC (pronounced \"nes-see\") code.

The hook `c-mode-common-hook' is run with no args at mode
initialization, then `nesc-mode-hook'.

Key bindings:
\\{nesc-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (c-initialize-cc-mode t)
  (set-syntax-table nesc-mode-syntax-table)
  (setq major-mode 'nesc-mode
	mode-name "nesC"
	local-abbrev-table nesc-mode-abbrev-table
	abbrev-mode t)
  (use-local-map c-mode-map)
  ;; `c-init-language-vars' is a macro that is expanded at compile
  ;; time to a large `setq' with all the language variables and their
  ;; customized values for our language.
  (c-init-language-vars nesc-mode)
  ;; `c-common-init' initializes most of the components of a CC Mode
  ;; buffer, including setup of the mode menu, font-lock, etc.
  ;; There's also a lower level routine `c-basic-common-init' that
  ;; only makes the necessary initialization to get the syntactic
  ;; analysis and similar things working.
  (c-common-init 'nesc-mode)
  (easy-menu-add nesc-menu)
  (run-hooks 'c-mode-common-hook)
  (run-hooks 'nesc-mode-hook)
  (c-update-modeline))


(provide 'nesc-mode)

;;; new-nesc.el ends here
