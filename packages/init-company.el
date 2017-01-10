;; Package configuration for company.

(use-package company
  :ensure t

  :bind 
  (:map company-active-map
    ("<escape>" . company-abort)))

(provide 'init-company)