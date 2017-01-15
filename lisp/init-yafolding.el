;; Package configuration for yafolding.

(use-package yafolding
  :ensure t
  :defer t

  :bind
  (:map  yafolding-mode-map
        ("M-<return>" . yafolding-toggle-element))
  
  :init
  (add-hook 'prog-mode-hook
            (lambda () (yafolding-mode)))
  
  (add-hook 'nxml-mode-hook
            (lambda () (yafolding-mode)))
  
  :config
  (setq yafolding-ellipsis-content "…"))

(provide 'init-yafolding)
