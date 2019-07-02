(use-package dumb-jump
  :defer t
  :straight t

  :config
  (setq dumb-jump-selector 'ivy)
  (setq dumb-jump-force-searcher 'rg)
  (setq dumb-jump-quiet t)
  (setq dumb-jump-aggressive nil))

(provide 'init-dumb-jump)
