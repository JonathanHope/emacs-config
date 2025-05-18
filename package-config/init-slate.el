;; -*- lexical-binding: t; -*-
(use-package slate
  :defer t
  :straight (slate :type git :host github :repo "jonathanhope/slate")

  :bind
  (:map slate-mode-map
        ("C-<tab>" . mainspring-hydra-slate/body))

  :commands (slate slate-refresh))

(provide 'init-slate)
