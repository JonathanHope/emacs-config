(use-package js
  :defer t

  :mode
  (("\\.json$" . js-mode))

  :bind
  (:map js-mode-map
        ("<return>". newline-and-indent))

  :init
  (setq js-indent-level 2))

(provide 'init-js-mode)
