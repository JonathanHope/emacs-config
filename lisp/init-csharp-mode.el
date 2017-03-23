;; Package configuration for csharp-mode

(use-package csharp-mode
  :ensure t
  :defer t

  :mode
  (("\\.cs$" . csharp-mode))

  :bind
  (:map csharp-mode-map
        ("<return>". block-newline-and-indent))

  :init
  (defun my-csharp-mode-hook ()
    (setq c-basic-offset 4)
    (electric-indent-mode 0)
    (electric-indent-local-mode 0))
  (add-hook 'csharp-mode-hook 'my-csharp-mode-hook)

  :config
  (setq-default c-electric-flag nil))

  (defun block-newline-and-indent (&optional arg)
    "Either inserts a newline and indents or adds two newlines and indents."
    (interactive "p")
    (if (and (and (char-after) (char-equal (char-after) ?}))
             (and (char-before) (char-equal (char-before) ?{)))
        (progn
          (newline-and-indent)
          (previous-line)
          (end-of-line)
          (newline-and-indent))
      (newline-and-indent)))

(provide 'init-csharp-mode)
