(use-package company
  :ensure t
  :defer t
  :diminish company-mode

  :bind
  (:map company-active-map
        ("<escape>" . company-abort))
  :init
  (progn
    (add-hook 'cider-repl-mode-hook #'company-mode)
    (add-hook 'cider-mode-hook #'company-mode))

  :config
  (setq company-idle-delay .3))

(provide 'init-company)
