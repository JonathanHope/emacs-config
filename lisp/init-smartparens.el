;; Package configuration for smartparens.

(use-package smartparens
  :ensure t
  :diminish smartparens-mode

  :init
  (add-hook 'emacs-lisp-mode-hook #'smartparens-strict-mode)
  (add-hook 'clojure-mode-hook #'smartparens-strict-mode)
  
  :config
  (setq sp-base-key-bindings 'paredit)

  (sp-use-paredit-bindings)

  (sp-with-modes sp-lisp-modes
    (sp-local-pair "'" nil :actions nil))

  (setq sp-navigate-consider-sgml-tags '(web-mode sgml-mode))

  (sp-with-modes '(web-mode sgml-mode)
    (sp-local-pair "<" ">"))

  (show-smartparens-global-mode t)

  (smartparens-global-mode t))

(provide 'init-smartparens)
