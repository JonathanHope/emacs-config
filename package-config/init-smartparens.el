(use-package smartparens
  :straight t

  :init
  (add-hook 'emacs-lisp-mode-hook #'smartparens-strict-mode)
  (add-hook 'clojure-mode-hook #'smartparens-strict-mode)
  ;; (add-hook 'org-mode-hook (lambda () (sp-local-pair 'org-mode "*" nil :actions :rem)))

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
