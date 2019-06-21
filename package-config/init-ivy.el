;; Package configuration for ivy.

(use-package ivy
  :ensure t

  :bind
  (:map ivy-minibuffer-map
        ("<tab>" . ivy-alt-done)
        ("S-<return>" . ivy-immediate-done))

  :config
  (ivy-mode 1)
  (setq ivy-extra-directories nil)
  (setq enable-recursive-minibuffers t)
  (setq ivy-height 20)
  (setq ivy-initial-inputs-alist nil)
  (setq ivy-count-format "")
  (setq ivy-re-builders-alist
        '((counsel-find-file . ivy--regex-fuzzy)
          (counsel-M-x . ivy--regex-fuzzy)
          (counsel-describe-variable . ivy--regex-fuzzy)
          (counsel-describe-function . ivy--regex-fuzzy)
          (ivy-switch-buffer . ivy--regex-fuzzy)
          (counsel-projectile-find-file . ivy--regex-fuzzy)
          (counsel-find-file-name . ivy--regex-fuzzy)
          (counsel-describe-face . ivy--regex-fuzzy)
          (t . ivy--regex-plus))))

(provide 'init-ivy)
