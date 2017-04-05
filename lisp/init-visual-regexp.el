;; Package configuration for visual-regexp.

(use-package pcre2el
  :ensure t)


(use-package visual-regexp
  :defer t)

(use-package visual-regexp-steroids
  :ensure t
  :demand t

  :commands (vr-replace-whole-buffer)

  :bind
  (:map vr/minibuffer-keymap
        ("S-<return>" . newline))

  :init
  (defun vr-replace-whole-buffer ()
    (interactive)
    (save-excursion
      (beginning-of-buffer)
      (call-interactively 'vr/replace)))

  :config
  (setq vr/engine 'pcre2el)
  (setq vr/auto-show-help nil))

(provide 'init-visual-regexp)
