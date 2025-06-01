;; -*- lexical-binding: t; -*-
(use-package tempel
  :straight t

  :bind
  (:map tempel-map
        ("<tab>" . tempel-next)
        ("<backtab>" . tempel-previous)
        ("M-RET" . tempel-done)))

(provide 'init-tempel)
