;; Package configuration for helm-swoop

(use-package helm-swoop
  :ensure t

  :init
  (defvar helm-swoop-split-window-function
    (lambda ($buf)
      (display-buffer $buf)))
  (setq helm-swoop-speed-or-color t))

(provide 'init-helm-swoop)
