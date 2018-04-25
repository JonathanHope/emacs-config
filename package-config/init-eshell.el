;; Package configuration for eshell.

(use-package "eshell"
  :defer t
  :commands (eshell)

  :config
  (setq eshell-prompt-function
        (lambda nil
          (concat
           (propertize "λ" 'face `(:foreground "#a3be8c" :weight bold))
           (propertize " " 'face `()))))
  (setq eshell-banner-message "")

  (defun eshell/clear ()
  "Clear terminal"
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer))))

(provide 'init-eshell)
