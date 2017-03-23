;; Package configuration for helm.

(use-package helm
  :ensure t
  :defer t

  :diminish helm-mode

  :bind
  (:map helm-map
        ("<tab>" . helm-execute-persistent-action)))

(use-package helm-config
  :defer t

  :init
  ;; Hide the helm mode line.
  (eval-after-load "helm"
    '(progn (defun helm-display-mode-line (source &optional force) (setq mode-line-format nil))
            (set-face-attribute 'helm-source-header nil :foreground (face-attribute 'helm-selection :background) :background (face-attribute 'helm-selection :background) :box nil :height 0.1)))

  :config
  ;; Hide the helm header.
  (setq helm-display-header-line nil)
  (setq helm-mode-line-string ""))

(provide 'init-helm)
