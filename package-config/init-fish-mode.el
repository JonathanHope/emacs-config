(use-package fish-mode
  :straight t
  :defer t

  :init
  (setq fish-indent-offset 2)

  :mode (("\\.fish$" . fish-mode)))

(provide 'init-fish-mode)
