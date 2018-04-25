;; Package configuration for async.

(use-package async)

(use-package dired-async
  :config
  (dired-async-mode 1))

(provide 'init-async)
