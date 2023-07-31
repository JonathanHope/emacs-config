(use-package corfu
  :straight t
  :defer t
  
  :hook ((typescript-ts-mode . corfu-mode)
         (cider-repl-mode-hook . corfu-mode)
         (cider-mode-hook . corfu-mode))

  :init
  (setq corfu-cycle t)
  (setq corfu-preview-current nil)

  :bind
  (:map corfu-mode-map
        ("C-;" . completion-at-point)))

(provide 'init-corfu)
