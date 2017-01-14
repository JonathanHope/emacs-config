;; Package configuration for helm.

(use-package helm
  :ensure t
  :diminish helm-mode
  
  :bind 
  (:map helm-map
        ("<tab>" . helm-execute-persistent-action)))

(use-package helm-config
  :config
  (helm-mode 1))

(provide 'init-helm)
