(if (string-match "^5.2[0-9]" c-version)
    (error "interface.el requires cc-mode 5.30 or later (http://cc-mode.sourceforge.net)"))

(require 'cl)
(require 'find-file)

(defun nesc-insert-interface-template ()
  (interactive)
  (push-mark)
  (end-of-line)
  (let* ((role (nesc-interface-role (point)))
	 (spec (nesc-interface-spec (point)))
	 (itype (car spec))
	 (iname (cadr spec))
	 (iparms (caddr spec)))
					;(message "type is %s, name is %s, role is %s, parms are %s" itype iname role iparms)
    (let ((idef (nesc-load-interface-buffer itype)))
      (word-search-forward "implementation")
      (forward-list)
      (backward-char)
      (nesc-interface-iterate itype idef
	 (lambda (command pre-name name post-name parms)
	   (if (or (and command (eq role 'provides))
		   (and (not command) (eq role 'uses)))
	       (insert (format "\n  %s%s.%s%s%s%s {\n    return SUCCESS;\n  }\n"
			       pre-name iname name post-name iparms parms))))
      ))))

(defun nesc-interface-iterate (name ibuffer fn)
  (let ((p (point))
	(m (mark))
	(b (current-buffer)))
    (switch-to-buffer ibuffer)
    (beginning-of-buffer)
    ;; find interface
    (while (not (progn (c-forward-comments) (looking-at "\\<interface\\>")))
      (if (eq (point-max) (point))
	  (error "%s is not an interface file" ibuffer))
      (c-forward-token-2))
    (c-forward-sexp) ; skip interface
    (c-forward-comments)
    ;; skip and check interface name
    (let* ((sname (point)) 
	   (ename (progn (c-forward-sexp) (point)))
	   (actual-name (buffer-substring sname ename)))
      (if (not (equal actual-name name))
	  (error "%s defines interface %s, not %s" ibuffer actual-name name)))
    (c-forward-comments)
    (if (not (looking-at "{"))
	(error "invalid interface in %s" ibuffer))
    (forward-char)

    ;; now we're inside the interface. iterate over all fns
    (while (not (progn (c-forward-comments) (looking-at "}")))
      ;; heuristic approach. assume parms list is last thing before ;
      ;; not necessarily true, e.g., for int (*x())[10], a function
      ;; returning a pointer to an array of 10 ints
      (let* ((s (point))
	     (e (progn (search-forward ";") (backward-char) (point)))
	     (command (save-excursion
			(word-search-backward "command" s t)))
	     (sargs (progn (backward-sexp) (point)))
	     (sname (progn (re-search-backward "\\<[a-zA-Z0-9_]+\\>") (point)))
	     (ename (progn (c-forward-sexp) (point))))
	(save-excursion
	  (let ((pre-name (buffer-substring s sname)) ; up to command, event name
		(name (buffer-substring sname ename)) ; command or event name
		(post-name (buffer-substring ename sargs)) ; up to parameter list
		(parms (buffer-substring sargs e))) ; parameter list
	    (switch-to-buffer b)
	    (goto-char p)
	    (set-mark m)
	    (apply fn (list command pre-name name post-name parms))
	    (setq b (current-buffer))
	    (setq p (point))
	    (setq m (mark))))
	(goto-char (+ e 1))))))
	      
    

(defun nesc-load-interface-buffer (type)
  (let ((fname (format "%s.nc" type)))
    (or 
     ;; option 1: a buffer whose name matches the interface's name
     (find-if (lambda (buf)
		(and (buffer-file-name buf)
		     (equal fname (file-name-nondirectory (buffer-file-name buf)))))
	      (buffer-list))
     ;; option 2: seach standard nesC directories
     (nesc-find-file fname)
     ;; option 3: ask the user
     (progn 
       (message "interface %s not found - please enter file name" type)
       (sit-for 1)
       (let* ((w (selected-window))
	      (b (save-excursion 
		   (call-interactively 'find-file-other-window))))
	 (select-window w)
	 b))
     )))

(setq tosroot (shell-command-to-string "ncc -print-tosdir"))
(setq tosroot (file-name-as-directory (substring tosroot 0 (- (length tosroot) 1))))

(defun nesc-subdirs (name) 
  (ff-all-dirs-under (concat tosroot name) '("." "..")))

(setq nesc-all-dirs
      (append
         (list (concat tosroot "interfaces")
	       (concat tosroot "types")
	       (concat tosroot "system"))
	 (nesc-subdirs "lib")
	 (nesc-subdirs "platform")
	 (nesc-subdirs "sensorboards")))

(defun nesc-find-file (name)
  (save-window-excursion
    (and (ff-get-file nesc-all-dirs name)
	 (current-buffer))))
     

(defun nesc-interface-spec (line-end)
  (save-excursion
    (beginning-of-line)
    (if (word-search-forward "interface" line-end t)
	(progn
	  (c-forward-comments)
	  (let* ((stype (point))
		 (etype (progn (c-forward-sexp) (point)))
		 (sname stype) ; default name is type's name
		 (ename etype)
		 (parms ""))
	    ;; check for as
	    (c-forward-comments)
	    (if (looking-at "\\<as\\>")
		(progn
		  (c-forward-sexp) ; skip as
		  (c-forward-comments)
		  (setq sname (point))
		  (c-forward-sexp)
		  (setq ename (point))
		  (c-forward-comments)))
	    (if (looking-at "\\[")
		(setq parms (nesc-interface-parameters)))
	    (list (buffer-substring stype etype)
		  (buffer-substring sname ename)
		  parms)))
      (error "no interface spec found"))))

(defun nesc-interface-parameters ()
  (let ((s (point)))
    (search-forward "]")
    (buffer-substring s (point))))
		  

(defun nesc-interface-role (line-end)
  (save-excursion
    (beginning-of-line)
    (or (nesc-provides-or-uses line-end)
	(progn
	  (search-backward "{")
	  (beginning-of-line)
	  (nesc-provides-or-uses line-end)))))

(defun nesc-provides-or-uses (limit)
  (cond ((word-search-forward "provides" limit t) 'provides)
	((word-search-forward "uses" limit t) 'uses)
	(t nil)))
