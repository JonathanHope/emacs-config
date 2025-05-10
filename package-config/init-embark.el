(use-package embark
  :straight t

  :init
   (setq embark-prompter #'embark-completing-read-prompter)
   (setq embark-indicators '(embark-minimal-indicator))
  
  :bind
  (:map minibuffer-local-map
        ("C-<tab>" . embark-act)))

(provide 'init-embark)
