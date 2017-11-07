;; Package configuration for doxygen.

(use-package doxygen
  :defer t
  :commands (doxygen-insert-comment doxygen-insert-file-comment doxygen-insert-function-comment))

(provide 'init-doxygen)
