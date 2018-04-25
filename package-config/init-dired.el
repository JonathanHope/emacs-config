;; Package configuration for dired.

(use-package "dired"
  :defer t

  :bind
  (:map dired-mode-map
        ("C-<tab>" . dired-hydra/body)))

(provide 'init-dired)
