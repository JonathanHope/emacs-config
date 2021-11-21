(use-package ibuffer
  :init
  (setq ibuffer-expert t)

  :bind
  (:map  ibuffer-mode-map
         ("C-<tab>" . mainspring-hydra-ibuffer/body)))

(provide 'init-ibuffer)
