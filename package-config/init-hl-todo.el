(use-package hl-todo
  :defer t
  :straight t

  :init
  (add-hook 'clojure-mode-hook #'hl-todo-mode)
  (add-hook 'csharp-mode-hook #'hl-todo-mode)

  (setq hl-todo-keyword-faces
        '(("TODO" . "#bf616a")))

  :config
  (defun helm-hl-todo-items ()
    "Show `hl-todo'-keyword items in buffer."
    (interactive)
    (ivy-read "TODO: "
              (keep-lines hl-todo--regexp)
              :require-match t
              :sort t
              :action '()))


  )

(provide 'init-hl-todo)
