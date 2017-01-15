;; Package configuration for isearch.

(use-package re-builder
  :defer t

  :init
  (setq reb-re-syntax 'string))

(provide 'init-re-builder)
