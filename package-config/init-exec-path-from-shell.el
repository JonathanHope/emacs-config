;; -*- lexical-binding: t; -*-

(use-package exec-path-from-shell
  :straight t
  
  :if (memq window-system '(mac ns x))

  :config
  (setq exec-path-from-shell-variables '("PATH" "GOPATH"))
  (exec-path-from-shell-initialize))

(provide 'init-exec-path-from-shell)
