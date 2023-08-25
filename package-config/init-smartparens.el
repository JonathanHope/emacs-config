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

  (show-smartparens-global-mode t)

  (smartparens-global-mode t))

(provide 'init-smartparens)
