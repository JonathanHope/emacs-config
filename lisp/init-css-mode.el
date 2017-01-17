;; Package configuration for css mode.

(use-package css-mode

  :mode (("\\.css$" . css-mode))
  
  :config
  (setq css-indent-offset '2))

(provide 'init-css-mode)
