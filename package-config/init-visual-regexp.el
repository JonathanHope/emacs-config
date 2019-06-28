(use-package visual-regexp
  :defer t)

(use-package visual-regexp-steroids
  :ensure t

  :commands (vr-replace-whole-buffer)

  :bind
  (:map vr/minibuffer-keymap
        ("S-<return>" . newline))

  :init
  (setq vr/engine 'pcre2el)
  (setq vr/auto-show-help nil)

  :config
  (defun vr-replace-whole-buffer ()
    (interactive)
    (save-excursion
      (beginning-of-buffer)
      (call-interactively 'vr/replace))))

(provide 'init-visual-regexp)
