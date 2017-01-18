;; Package configuration for web mode.

(use-package web-mode
  :ensure t
  :defer t

  :mode (("\\.html\\'" . web-mode))

  :bind
  (:map web-mode-map
        ("C-<tab>" . html-hydra/body))
  
  :init
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-style-padding 2)
  (setq web-mode-script-padding 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2))

(provide 'init-web-mode)
