(use-package smartparens
  :straight t

  :config
  (require 'smartparens-config)

  (setq sp-base-key-bindings 'paredit)

  (sp-use-paredit-bindings)

  (unbind-key "M-<up>" smartparens-mode-map)
  (unbind-key "M-<down>" smartparens-mode-map)

  (sp-with-modes sp-lisp-modes
    (sp-local-pair "'" nil :actions nil))

  (setq sp-navigate-consider-sgml-tags '(sgml-mode))

  (sp-with-modes '(sgml-mode)
    (sp-local-pair "<" ">"))

  :hook ((typescript-ts-mode . show-smartparens-mode)
         (typescript-ts-mode . smartparens-mode)
         (tsx-ts-mode . show-smartparens-mode)
         (tsx-ts-mode . smartparens-mode)
         (js-ts-mode . show-smartparens-mode)
         (js-ts-mode . smartparens-mode)
         (go-ts-mode . show-smartparens-mode)
         (go-ts-mode . smartparens-mode)
         (org-mode . smartparens-mode)
         (org-mode . show-smartparens-mode)
         (elisp-mode . smartparens-mode)
         (elisp-mode . show-smartparens-mode)))

(provide 'init-smartparens)
