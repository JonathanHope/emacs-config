;; Package configuration for company.

(use-package company
  :ensure t

  :bind 
  (:map company-active-map
        ("<escape>" . company-abort))
  :init
  (add-hook 'cider-repl-mode-hook 'company-mode)
  (add-hook 'cider-mode-hook 'company-mode))

(provide 'init-company)
