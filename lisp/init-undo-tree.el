;; Package configuration for undo-tree.

(use-package undo-tree
  :ensure t
  
  :config 
  (global-undo-tree-mode))

(provide 'init-undo-tree)