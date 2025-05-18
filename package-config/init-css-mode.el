;; -*- lexical-binding: t; -*-
(use-package css-ts-mode
  :defer t

  :init
  (setq css-indent-offset 2)
  
  :mode
  (("\\.css$" . css-ts-mode)))

(provide 'init-css-mode)
