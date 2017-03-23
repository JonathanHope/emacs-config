;; Package configuration for css mode.

(use-package css-mode
  :defer t

  :mode (("\\.css$" . css-mode))

  :bind
  (:map css-mode-map
        ("<return>". newline-and-indent))

  :config
  (setq css-indent-offset '2))

(provide 'init-css-mode)
