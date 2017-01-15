;; Package configuration for dired.

(use-package "dired"
  :bind 
  (:map dired-mode-map
        ("?" . dired-hydra/body)))

(provide 'init-dired)
