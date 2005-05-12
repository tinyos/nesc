;;; necs.el --- nesC mode

;; Author: David Gay <dgay@intel-research.net>
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

(require 'cc-mode)

;; Load appropriate version of nesC emacs mode
(if (string-match "^5.2[0-9]" c-version)
    (load-library "old-nesc.el")
  (load-library "new-nesc.el"))

;; c-font-lock-declarators has a bug (spams a warning message) when
;; editing enum's with '=' in font-lock-mode, in 5.30.9. Redefine it.
(if (string-equal c-version "5.30.9")
    (defun c-font-lock-declarators (limit list types)
      ;; Assuming the point is at the start of a declarator in a
      ;; declaration, fontify it.  If LIST is non-nil, fontify also all
      ;; following declarators in a comma separated list (e.g.  "foo" and
      ;; "bar" in "int foo = 17, bar;").  Stop at LIMIT.  If TYPES is
      ;; non-nil, fontify all identifiers as types.  Nil is always
      ;; returned.

      ;;(message "c-font-lock-declarators from %s to %s" (point) limit)
      (c-fontify-types-and-refs
       ((pos (point)) next-pos id-start id-end
	paren-depth
	id-face got-init
	c-last-identifier-range
	(separator-prop (if types 'c-decl-type-start 'c-decl-id-start)))

       (while (and
	       pos
	       (< (point) limit)

	       (let (got-identifier)
		 (setq paren-depth 0)
		 ;; Skip over type decl prefix operators.  (Note similar
		 ;; code in `c-font-lock-declarations'.)
		 (while (and (looking-at c-type-decl-prefix-key)
			     (if (and (c-major-mode-is 'c++-mode)
				      (match-beginning 2))
				 ;; If the second submatch matches in C++ then
				 ;; we're looking at an identifier that's a
				 ;; prefix only if it specifies a member pointer.
				 (progn
				   (setq id-start (point))
				   (c-forward-name)
				   (if (looking-at "\\(::\\)")
				       ;; We only check for a trailing "::" and
				       ;; let the "*" that should follow be
				       ;; matched in the next round.
				       t
				     ;; It turned out to be the real identifier,
				     ;; so flag that and stop.
				     (setq got-identifier t)
				     nil))
			       t))
		   (if (eq (char-after) ?\()
		       (progn
			 (setq paren-depth (1+ paren-depth))
			 (forward-char))
		     (goto-char (match-end 1)))
		   (c-forward-syntactic-ws))

		 ;; If we didn't pass the identifier above already, do it now.
		 (unless got-identifier
		   (setq id-start (point))
		   (c-forward-name))
		 (setq id-end (point))

		 (/= id-end pos))

	       ;; Skip out of the parens surrounding the identifier.
	       (or (= paren-depth 0)
		   (c-safe (goto-char (scan-lists (point) 1 paren-depth))))

	       (<= (point) limit)

	       ;; Search syntactically to the end of the declarator (";",
	       ;; ",", a closen paren, eob etc) or to the beginning of an
	       ;; initializer or function prototype ("=" or "\\s\(").
	       ;; Note that the open paren will match array specs in
	       ;; square brackets, and we treat them as initializers too.
	       (c-syntactic-re-search-forward
		"[;,]\\|\\s)\\|\\'\\|\\(=\\|\\s(\\)" limit t t))

	 (setq next-pos (match-beginning 0)
	       id-face (if (eq (char-after next-pos) ?\()
			   'font-lock-function-name-face
			 'font-lock-variable-name-face)
	       got-init (and (match-beginning 1)
			     (char-after (match-beginning 1))))

	 (if types
	     ;; Register and fontify the identifer as a type.
	     (let ((c-promote-possible-types t))
	       (goto-char id-start)
	       (c-forward-type))
	   ;; Fontify the last symbol in the identifier if it isn't fontified
	   ;; already.  The check is necessary only in certain cases where this
	   ;; function is used "sloppily", e.g. in `c-simple-decl-matchers'.
	   (when (and c-last-identifier-range
		      (not (get-text-property (car c-last-identifier-range)
					      'face)))
	     (c-put-font-lock-face (car c-last-identifier-range)
				   (cdr c-last-identifier-range)
				   id-face)))

	 (goto-char next-pos)
	 (setq pos nil)
	 (when list
	   ;; Jump past any initializer or function prototype to see if
	   ;; there's a ',' to continue at.

	   (cond ((eq id-face 'font-lock-function-name-face)
		  ;; Skip a parenthesized initializer (C++) or a function
		  ;; prototype.
		  (if (c-safe (c-forward-sexp 1) t)
		      (c-forward-syntactic-ws limit)
		    (goto-char limit)))

		 (got-init
		  ;; Skip an initializer expression.  If we're at a '='
		  ;; then accept a brace list directly after it to cope
		  ;; with array initializers.  Otherwise stop at braces
		  ;; to avoid going past full function and class blocks.
		  (and (if (and (eq got-init ?=)
				(= (c-forward-token-2 nil nil limit) 0)
				(looking-at "{"))
			   (c-safe
			    (let ((p (point)))
			      (c-forward-sexp)
			      (if (> (point) limit)
				  (goto-char p)))
			    t)
			 t)
		       (c-syntactic-re-search-forward "[;,{]" limit 'move t)
		       (backward-char)))

		 (t (c-forward-syntactic-ws limit)))

	   ;; If a ',' is found we set pos to the next declarator and iterate.
	   (when (and (< (point) limit) (looking-at ","))
	     (c-put-char-property (point) 'c-type separator-prop)
	     (forward-char)
	     (c-forward-syntactic-ws limit)
	     (setq pos (point))))))
      nil))
