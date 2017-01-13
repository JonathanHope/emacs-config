;; Package configuration for nxml-mode.

(use-package "nxml-mode"
  :bind 
  (:map nxml-mode-map
        ("M-<left>" . nxml-backward-tag)
        ("M-<right>" . nxml-forward-tag)
        ("<tab>" . nxml-finish-element-new-line)
        ("<backtab>" . nxml-finish-element-same-line)
        ("<return>" . nxml-newline-and-indent))

  :init
  (setq auto-mode-alist
        (cons
         '("\\.\\(xml\\|edmx\\|config\\)\\'"
           . nxml-mode)
         auto-mode-alist))

  :config
  (defun nxml-forward-tag (&optional arg)
    (interactive "p")
    (while (progn
             (nxml-forward-single-balanced-item)
             (> (setq arg (1- arg)) 0))))

  (defun nxml-backward-tag (&optional arg)
    (interactive "p")
    (while (progn
             (nxml-backward-single-balanced-item)
             (< (setq arg (1+ arg)) 0))))

  (defun nxml-finish-element-new-line ()
    "Wrap an arbitrary identifier in brackets, complete it, create a new line, and apply indentation."
    (interactive)
    (sp-backward-sexp)
    (insert "<")
    (sp-forward-sexp)
    (insert ">")
    (nxml-finish-element)
    (forward-line -1)
    (end-of-line)
    (newline-and-indent))

  (defun move-line-up ()
  "Removes leading spaces from the current line, and then moves the current line to the end of the previous line."
  (interactive)
  (let (start end)
    (save-excursion
      (beginning-of-line)
      (let ((search-end (save-excursion (end-of-line) (point))))
        (re-search-forward "[^[:space:]]" search-end))
      (setq end (1- (point)))
      (previous-line)
      (end-of-line)
      (setq start (point))
      (delete-region start end))
    (goto-char start)))

  (defun nxml-finish-element-same-line ()
    "Wrap an arbitrary identifier in brackets and complete the tag."
    (interactive)
    (sp-backward-sexp)
    (insert "<")
    (sp-forward-sexp)
    (insert ">")
    (nxml-finish-element)
    (move-line-up))

  (defun nxml-newline-and-indent ()
    "Either inserts a newline and indents or adds two newlines and indents."
    (interactive)
    (if (and (char-equal (char-after) ?<)
             (char-equal (char-before) ?>))
        (progn
          (newline-and-indent)
          (previous-line)
          (sp-down-sexp)
          (newline-and-indent))
      (newline-and-indent))))

(provide 'init-nxml-mode)
