;;;###autoload
(define-generic-mode 'ebnf-mode
  '(("(*" . "*)"))
  '("=" "|" "/" ":" "::=" "=")
  '(("['\"].*?['\"]" . font-lock-string-face)
    ("*\\|+\\|#\\|!\\|&\\|,\\|;" . font-lock-keyword-face)
    ("(\\|)\\|<\\|>\\|{\\|}" . font-lock-type-face)
    ("[A-Za-z0-0_\\-]+" . font-lock-variable-name-face))
  '("\\.ebnf\\'")
  `(,(lambda () (setq mode-name "EBNF")))
  "Major mode for EBNF syntax highlighting.")

(provide 'ebnf-mode)
