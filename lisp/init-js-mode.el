(use-package js
  :defer t
  :mode 
  (("\\.js$" . js-mode)
   ("\\.json$" . js-mode))
  :bind
  (:map js-mode-map
        ("C-<tab>" . javascript-hydra/body))

  :init
  (setq js-indent-level 2))


(provide 'init-js-mode)
