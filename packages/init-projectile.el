;; Package configuration for projectile.

(use-package projectile
  :ensure t
  
  :config
  (setq projectile-completion-system 'helm)
  (setq projectile-indexing-method 'alien)
  (projectile-global-mode))

(provide 'init-projectile)