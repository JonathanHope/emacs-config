;; Package configuration for visual-regexp.

(use-package pcre2el
  :ensure t

  :init
  (defun vr-replace-whole-buffer ()
    (interactive)
    (save-excursion
      (beginning-of-buffer)
      (call-interactively 'vr/replace))))

(use-package visual-regexp-steroids
  :ensure t
  :defer t

  :commands (vr-replace-whole-buffer)

  :config
  (setq vr/engine 'pcre2el))

(provide 'init-visual-regexp)
