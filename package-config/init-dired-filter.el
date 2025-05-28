;; -*- lexical-binding: t; -*-
(use-package dired-filter
  :straight t
  :defer t

  :hook (dired-mode . dired-filter-mode)

  :init
  (setq dired-filter-stack '()))

(provide 'init-dired-filter)
