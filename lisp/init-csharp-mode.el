;; Package configuration for csharp-mode

(use-package csharp-mode
  :defer t
  :ensure t
  :mode
  (("\\.cs$" . csharp-mode))

  :init
  (defun my-csharp-mode-hook ()
    (setq c-basic-offset 4)
    (electric-indent-mode 0)
    (electric-indent-local-mode 0))
  (add-hook 'csharp-mode-hook 'my-csharp-mode-hook)

  :config
  (setq-default c-electric-flag nil))

(provide 'init-csharp-mode)
