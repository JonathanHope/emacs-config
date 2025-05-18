;; -*- lexical-binding: t; -*-
(use-package simpleclip
  :defer t
  :straight t
  
  :commands (simpleclip-cut simpleclip-copy simpleclip-paste)

  :config
  (simpleclip-mode 1))

(provide 'init-simpleclip)
