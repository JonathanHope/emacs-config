(use-package "dired"
  :defer t

  :bind
  (:map dired-mode-map
        ("C-<tab>" . mainspring-hydra-dired/body)
        ("<backspace>" . dired-up-directory))

  :init
  (defun dired-mode-setup ()
    (dired-hide-details-mode 1))
  (add-hook 'dired-mode-hook 'dired-mode-setup))

(provide 'init-dired)
