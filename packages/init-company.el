;; Package configuration for company.

(use-package company
  :ensure t
  :defer t

  :bind 
  (:map company-active-map
        ("<escape>" . company-abort))
  :init
  (add-hook 'cider-repl-mode-hook 'company-mode)
  (add-hook 'cider-mode-hook 'company-mode)
  (add-hook 'js3-mode-hook 'company-mode)
  (add-hook 'nxml-mode-hook 'company-mode)

  :config
  (add-to-list 'company-backends 'company-tern))

(provide 'init-company)
