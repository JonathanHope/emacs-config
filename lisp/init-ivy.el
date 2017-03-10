;; Package configuration for ivy.

(use-package ivy
  :ensure t
  :diminish ivy-mode

  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (setq counsel-ag-base-command "ag --vimgrep --nocolor --nogroup %s")
  (setq ivy-height 20)
  (setq ivy-initial-inputs-alist nil)
  (setq ivy-count-format "")
  (setq ivy-re-builders-alist
        '((counsel-find-file . ivy--regex-fuzzy)
          (counsel-M-x . ivy--regex-fuzzy)
          (counsel-describe-variable . ivy--regex-fuzzy)
          (counsel-describe-function . ivy--regex-fuzzy)
          (ivy-switch-buffer . ivy--regex-fuzzy)
          (counsel-projectile . ivy--regex-fuzzy)
          (t . ivy--regex-plus)))
  )

(provide 'init-ivy)
