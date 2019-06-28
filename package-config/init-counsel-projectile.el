;; Package configuration for counsel-projectile.

(use-package counsel-projectile
  :ensure t

  :init
  (setq counsel-projectile-sort-files t)
  (setq counsel-projectile-find-file-matcher 'counsel--find-file-matcher))

(provide 'init-counsel-projectile)
