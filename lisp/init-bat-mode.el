;; Package configuration for bat-mode

(use-package bat-mode
  :defer t
  :mode
  (("\\.bat$" . bat-mode))
  :bind
  (:map bat-mode-map
        ("C-<tab>" . bat-hydra/body)))


(provide 'init-bat-mode)
