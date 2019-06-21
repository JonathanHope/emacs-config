;; Package configuration for dired.

(use-package "dired"
  :defer t

  :bind
  (:map dired-mode-map
        ("C-<tab>" . dired-hydra/body))

  :init
  (defun dired-mode-setup ()
    (dired-hide-details-mode 1))
  (add-hook 'dired-mode-hook 'dired-mode-setup))

(provide 'init-dired)
