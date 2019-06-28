(use-package projectile
  :ensure t

  :init
  (setq projectile-switch-project-action 'counsel-projectile-find-file-or-buffer)
  (setq projectile-indexing-method 'alien)
  (setq projectile-enable-caching nil)
  (setq projectile-require-project-root nil)
  (setq projectile-completion-system 'ivy)
  (setq projectile-require-project-root t)

  :config
  (projectile-mode))

(provide 'init-projectile)
