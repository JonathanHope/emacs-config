;; Package configuration for slate.

(use-package slate
  :defer t

  :bind
  (:map slate-mode-map
        ("C-<tab>" . slate-hydra/body))

  :commands (slate slate-refresh))

(provide 'init-slate)
