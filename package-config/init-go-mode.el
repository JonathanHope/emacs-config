(use-package go-ts-mode
  :defer t

  :mode (("\\.go$" . go-ts-mode))

  :bind
  (:map go-ts-mode-map
        ("C-<tab>" . mainspring-hydra-go/body))
  
  :init
  (setq go-ts-mode-indent-offset 2)
  
  :hook
  (go-ts-mode . eglot-ensure))

(provide 'init-go-mode)
