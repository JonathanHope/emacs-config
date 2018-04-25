;; Package configuration for sgml related modes.

(use-package sgml-mode
  :defer t

  :mode (("\\.xml$" . sgml-mode)
         ("\\.config$" . sgml-mode)
         ("\\.csproj$" . sgml-mode)
         ("\\.sln$" . sgml-mode))

  :bind
  (:map sgml-mode-map
        ("<return>" . tag-newline-and-indent)
        ("<tab>" . xml-finish-element-new-line)
        ("<backtab>" . xml-finish-element-same-line))

  :config
  (defun backward-symbol (&optional arg)
    "Move backward until encountering the beginning of a symbol."
    (interactive "p")
    (forward-symbol (- (or arg 1))))

  (defun forward-start-end-tag (&optional arg)
    "Move forward over a tag start or end."
    (interactive "p")
    (forward-symbol arg)
    (forward-char))

  (defun backward-start-end-tag (&optional arg)
    "Move backward over a tag start or end."
    (interactive "p")
    (backward-symbol arg)
    (backward-char))

  (defun tag-newline-and-indent (&optional arg)
    "Either inserts a newline and indents or adds two newlines and indents."
    (interactive "p")
    (if (and (and (char-after) (char-equal (char-after) ?<))
             (and (char-before) (char-equal (char-before) ?>)))
        (progn
          (newline-and-indent)
          (previous-line)
          (forward-start-end-tag arg)
          (newline-and-indent))
      (newline-and-indent)))

  (defun xml-finish-element-new-line (&optional arg)
    "Wrap an arbitrary identifier in brackets, complete it, create a new line, and apply indentation."
    (interactive "p")
    (when (string-match "[A-Za-z0-9\_\.\-]" (char-to-string (char-before)))
      (backward-symbol arg)
      (insert "<")
      (forward-symbol arg)
      (insert ">")
      (sgml-close-tag)
      (backward-start-end-tag arg)
      (tag-newline-and-indent arg)))

  (defun xml-finish-element-same-line (&optional arg)
    "Wrap an arbitrary identifier in brackets and complete the tag."
    (interactive "p")
    (when (string-match "[A-Za-z0-9\_\.\-]" (char-to-string (char-before)))
      (backward-symbol arg)
      (insert "<")
      (forward-symbol arg)
      (insert ">")
      (sgml-close-tag)
      (backward-start-end-tag arg))))

(provide 'init-sgml-mode)
