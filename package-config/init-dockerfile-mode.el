(use-package dockerfile-mode
  :defer t
  :straight t

  :mode (("Dockerfile\\'" . dockerfile-mode))

  :bind
  (:map dockerfile-mode-map
        ("C-<tab>" . mainspring-hydra-dockerfile/body)))

(provide 'init-dockerfile-mode)
