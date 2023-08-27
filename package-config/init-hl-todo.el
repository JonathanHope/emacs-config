(use-package hl-todo
  :straight t

  :init
  (global-hl-todo-mode)

  (setq hl-todo-keyword-faces
        '(("TODO" . "#bf616a"))))

(provide 'init-hl-todo)
