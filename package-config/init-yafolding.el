;; Package configuration for yafolding.

(use-package yafolding
  :ensure t
  :defer t

  :bind
  (:map  yafolding-mode-map
         ("M-<return>" . yafolding-toggle-element))

  :config
  (setq yafolding-ellipsis-content "…"))

(provide 'init-yafolding)
