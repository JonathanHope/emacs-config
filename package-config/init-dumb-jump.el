(use-package dumb-jump
  :ensure t
  :defer t

  :config
  (setq dumb-jump-selector 'ivy)
  (setq dumb-jump-force-searcher 'rg)
  (setq dumb-jump-quiet t)
  (setq dumb-jump-aggressive nil))

(provide 'init-dumb-jump)
