;; Package configuration for undo-tree.

(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode

  :init
  (global-undo-tree-mode t)
  ;; TODO: Why do I need this?
  (add-hook 'find-file-hook 'undo-tree-mode))

(provide 'init-undo-tree)
