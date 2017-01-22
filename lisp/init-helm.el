;; Package configuration for helm.

(use-package helm
  :ensure t
  :diminish helm-mode
  
  :bind 
  (:map helm-map
        ("<tab>" . helm-execute-persistent-action)))

(use-package helm-config
  :config
  (setq helm-M-x-fuzzy-match t
        helm-buffers-fuzzy-matching t)
  
  (helm-mode 1))

(provide 'init-helm)
