;; Package configuration for counsel-projectile.

(use-package counsel-projectile
  :ensure t

  :config
  (setq projectile-switch-project-action 'counsel-projectile-find-file-or-buffer))

(provide 'init-counsel-projectile)
