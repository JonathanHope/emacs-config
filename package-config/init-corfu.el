(use-package corfu
  :straight (:files (:defaults "extensions/*.el"))
  :defer t
  
  :hook ((typescript-ts-mode . corfu-mode)
         (typescript-ts-mode . corfu-popupinfo-mode)
         (tsx-ts-mode . corfu-mode)
         (tsx-ts-mode . corfu-popupinfo-mode)
         (js-ts-mode . corfu-mode)
         (js-ts-mode . corfu-popupinfo-mode)
         (cider-repl-mode-hook . corfu-mode)
         (cider-repl-mode-hook . corfu-popupinfo-mode)
         (cider-mode-hook . corfu-mode)
         (cider-mode-hook . corfu-popupinfo-mode))

  :init
  (setq corfu-cycle t)
  (setq corfu-preview-current nil)

  :bind
  (:map corfu-mode-map
        ("C-;" . completion-at-point))

  :config
  (use-package corfu-popupinfo
    :straight nil

    :init
    (setq corfu-popupinfo-max-height 20)))

(provide 'init-corfu)
