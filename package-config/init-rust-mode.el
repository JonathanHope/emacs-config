;; Package configuration for rust-mode.

(use-package rust-mode
  :defer t
  :ensure t

  :mode (("\\.rs$" . rust-mode))

  :config
  (setq rust-indent-offset 2))

(provide 'init-rust-mode)
