;; Package configuration for visual-regexp.

(use-package pcre2el
  :ensure t

  :config
  (defun vr-replace-whole-buffer ()
    (interactive)
    (save-excursion
      (beginning-of-buffer)
      (call-interactively 'vr/replace))))

(use-package visual-regexp-steroids
  :ensure t

  :config
  (setq vr/engine 'pcre2el))

(provide 'init-visual-regexp)
