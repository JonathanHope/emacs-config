;; Package configuration for dumb-jump.

(use-package dumb-jump
  :ensure t
  :defer t

  :config
  (setq dumb-jump-selector 'ivy))

(provide 'init-dumb-jump)
