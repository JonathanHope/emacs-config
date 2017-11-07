;; Package configuration for rust-mode.

(use-package rust-mode
  :defer t
  :ensure t

  :mode (("\\.rs$" . rust-mode)))

(provide 'init-rust-mode)
