(use-package typescript-ts-mode
  :defer t

  :mode (("\\.ts$" . typescript-ts-mode))

  :hook (typescript-ts-mode . eglot-ensure))

(provide 'init-typescript-mode)
