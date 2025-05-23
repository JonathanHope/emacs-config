;; -*- lexical-binding: t; -*-
(use-package winum
  :straight t
  :defer 0

  :bind
  (:map winum-keymap
        ("M-0" . winum-select-window-0-or-10)
        ("M-1" . winum-select-window-1)
        ("M-2" . winum-select-window-2)
        ("M-3" . winum-select-window-3)
        ("M-4" . winum-select-window-4)
        ("M-5" . winum-select-window-5)
        ("M-6" . winum-select-window-6)
        ("M-7" . winum-select-window-7)
        ("M-8" . winum-select-window-8)
        ("M-9" . winum-select-window-9))

  :config
  (setq winum-format "%s")
  (winum-mode))

(provide 'init-winum)
