;; Package configuration for mainspring-graphviz-dot-mode.

(use-package mainspring-graphviz-dot-mode
  :defer t

  :mode (("\\.gv$" . mainspring-graphviz-dot-mode)))

(provide 'init-mainspring-graphviz-dot-mode)
