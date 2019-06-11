;;;###autoload
(define-generic-mode 'mainspring-graphviz-dot-mode
  '(("/*" . "*/"))
  '()
  '(("^digraph" . font-lock-keyword-face)
    ("->" . font-lock-keyword-face)
    ("graph " . font-lock-type-face)
    ("node " . font-lock-type-face)
    ("edge " . font-lock-type-face)
    ("bgcolor" . font-lock-variable-name-face)
    ("resolution" . font-lock-variable-name-face)
    ("fontname" . font-lock-variable-name-face)
    ("fontcolor" . font-lock-variable-name-face)
    ("fontsize" . font-lock-variable-name-face))
  '("\\.gv\\'")
  `(,(lambda () (setq mode-name "Dot Dir. Graph")))
  "Major mode for GraphViz Dot directed graph syntax highlighting.")

(provide 'mainspring-graphviz-dot-mode)
