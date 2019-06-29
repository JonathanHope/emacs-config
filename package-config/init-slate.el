(use-package slate
  :defer t

  :bind
  (:map slate-mode-map
        ("C-<tab>" . mainspring-hydra-slate/body))

  :commands (slate slate-refresh))

(provide 'init-slate)
