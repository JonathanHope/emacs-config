;; -*- lexical-binding: t; -*-
(use-package ox-reveal
  :straight t
  :defer t

  :init
  (setq org-reveal-root "https://cdn.jsdelivr.net/npm/reveal.js")

  :commands (org-reveal-export-to-html))

(provide 'init-ox-reveal)
