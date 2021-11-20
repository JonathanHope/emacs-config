(use-package projectile
  :straight t

  :init
  (setq projectile-indexing-method 'alien)
  (setq projectile-enable-caching nil)
  (setq projectile-completion-system 'default)
  (setq projectile-require-project-root 'prompt)

  :config
  (projectile-mode))

(provide 'init-projectile)
