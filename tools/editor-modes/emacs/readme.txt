This nesC mode supports both cc-mode 5.28 and 5.30.x, so should work with
both emacs and xemacs. Send bug reports (with full emacs and cc-mode version
details) to dgay@intel-research.net. The cc-mode version can be found
by executing `M-x c-version'.

To use this nesC mode for emacs, you need to:
- copy *nesc.el to some directory X (I use ~/lib/emacs/lisp)
- add the following to your .emacs file:

(setq load-path (cons (expand-file-name "X") load-path))
(autoload 'nesc-mode "nesc.el")
(add-to-list 'auto-mode-alist '("\\.nc\\'" . nesc-mode))

David Gay
dgay@intel-research.net
