(use-package hl-todo
  :defer t
  :straight t

  :init
  (add-hook 'clojure-mode-hook #'hl-todo-mode)
  (add-hook 'csharp-mode-hook #'hl-todo-mode)

  (setq hl-todo-keyword-faces
        '(("TODO" . "#bf616a"))))

(provide 'init-hl-todo)
