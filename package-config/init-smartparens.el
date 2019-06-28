(use-package smartparens
  :ensure t

  :init
  (add-hook 'emacs-lisp-mode-hook #'smartparens-strict-mode)
  (add-hook 'clojure-mode-hook #'smartparens-strict-mode)

  :config
  (setq sp-base-key-bindings 'paredit)

  (sp-use-paredit-bindings)

  (unbind-key "M-<up>" smartparens-mode-map)
  (unbind-key "M-<down>" smartparens-mode-map)

  (sp-with-modes sp-lisp-modes
    (sp-local-pair "'" nil :actions nil))

  (sp-with-modes '(org-mode)
    (sp-local-pair "(" nil :actions nil)
    (sp-local-pair "[" nil :actions nil))

  (setq sp-navigate-consider-sgml-tags '(sgml-mode))

  (sp-with-modes '(sgml-mode)
    (sp-local-pair "<" ">"))

  (show-smartparens-global-mode t)

  (smartparens-global-mode t))

(provide 'init-smartparens)
