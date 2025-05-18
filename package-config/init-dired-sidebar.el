;; -*- lexical-binding: t; -*-
(use-package dired-sidebar
  :straight t
  :commands (dired-sidebar-toggle-sidebar)

  :after (mainspring-mode-line)
  
  :init
  (setq  dired-sidebar-use-custom-modeline nil))

(provide 'init-dired-sidebar)
