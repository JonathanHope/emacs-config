(use-package undo-tree
  :straight t

  :init
  (with-eval-after-load 'undo-tree (defun undo-tree-overridden-undo-bindings-p () nil))
  (global-undo-tree-mode))

(provide 'init-undo-tree)
