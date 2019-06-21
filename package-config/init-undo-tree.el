;; Package configuration for undo-tree.

(use-package undo-tree
  :ensure t

  :init
  (global-undo-tree-mode t))

(provide 'init-undo-tree)
