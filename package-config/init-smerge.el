(use-package smerge-mode
  :defer t

  :bind
  (:map smerge-mode-map
        ("C-<tab>" . mainspring-hydra-smerge/body)))

(provide 'init-smerge)
