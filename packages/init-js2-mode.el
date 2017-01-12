(use-package js2-mode
  :ensure t
  :defer t
  :mode (("\\.js$" . js2-mode))
  :commands js2-mode

  :bind
  (:map js2-mode-map
        ("C-<tab>" . javascript-hydra/body))
  
  :config
  (setq-default
   js2-basic-offset 2
   js2-highlight-external-variables t
   js2-mode-show-parse-errors t
   js2-mode-show-strict-warnings t)

  (add-hook 'js2-mode-hook 'skewer-mode))

(provide 'init-js2-mode)
