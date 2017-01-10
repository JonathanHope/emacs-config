; Package configuration for paredit.

(use-package paredit
  :ensure t
  :bind
  (:map  paredit-mode-map
    ("M-<up>" . nil)
    ("M-<down>" . nil)))

(provide 'init-paredit)