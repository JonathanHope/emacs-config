(use-package go-ts-mode
  :defer t

  :mode (("\\.go$" . go-ts-mode))

  :init
  (setq go-ts-mode-indent-offset 2)
  
  :hook
  (go-ts-mode . eglot-ensure))

(provide 'init-go-mode)
