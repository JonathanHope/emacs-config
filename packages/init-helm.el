;; Package configuration for helm.

(use-package helm
  :ensure t
  :bind 
  (:map helm-map
    ("<tab>" . helm-execute-persistent-action)))

(use-package helm-config
  :config)

(use-package helm-projectile
  :ensure t)

(provide 'init-helm)