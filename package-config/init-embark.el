(use-package embark
  :straight t

  :bind
  (:map minibuffer-local-map
        ("C-<tab>" . embark-act)))

(provide 'init-embark)
