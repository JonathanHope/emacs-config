;; Package configuration for projectile.

(use-package projectile
  :ensure t

  :config
  (setq projectile-switch-project-action 'counsel-projectile-find-file-or-buffer)
  (setq projectile-indexing-method 'alien)
  (projectile-global-mode))

(provide 'init-projectile)
