;; -*- lexical-binding: t; -*-
(use-package pinentry
  :straight t

  :init
  (setq epa-pinentry-mode 'loopback)
  (pinentry-start))

(provide 'init-pinentry)
