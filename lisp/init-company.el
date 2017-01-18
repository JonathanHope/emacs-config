;; Package configuration for company.

(use-package company
  :ensure t
  :defer t
  :diminish company-mode

  :bind 
  (:map company-active-map
        ("<escape>" . company-abort))
  :init
  (progn
    (add-hook 'prog-mode-hook #'company-mode)
    (add-hook 'sgml-mode-hook #'company-mode)
    (add-hook 'web-mode-hook #'company-mode))

  :config
  (add-to-list 'company-backends 'company-tern))

(provide 'init-company)
