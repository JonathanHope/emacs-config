;; Package configuration for dired.

(use-package "dired"
  :defer t

  :bind
  (:map dired-mode-map
        ("?" . dired-hydra/body)))

(provide 'init-dired)
