(use-package magit
  :ensure t
  :bind 
  (:map with-editor-mode-map
    ("M-c" . with-editor-finish)))

(provide 'init-magit)