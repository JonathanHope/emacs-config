(use-package js3-mode
  :ensure t
  :defer t
  :mode (("\\.js$" . js3-mode))
  :commands js3-mode

  :bind
  (:map js3-mode-map
        ("C-<tab>" . javascript-hydra/body)))

(provide 'init-js3-mode)
