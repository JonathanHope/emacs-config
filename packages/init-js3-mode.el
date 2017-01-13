(use-package js3-mode
  :ensure t
  :defer t
  :mode (("\\.js$" . js3-mode))
  :commands js3-mode

  :bind
  (:map js3-mode-map
        ("C-<tab>" . javascript-hydra/body))

  :init
  (setq
   js3-auto-indent-p t         
   js3-enter-indents-newline t 
   js3-indent-on-enter-key t))


(provide 'init-js3-mode)
