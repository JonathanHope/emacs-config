;; Package configuration for zoom.

(use-package zoom
  :ensure t

  :config
  (zoom-mode 1)
  (setq zoom-size '(0.618 . 0.618))
  (setq zoom-ignored-major-modes '(magit-mode magit-popup-mode))
  (setq zoom-ignored-buffer-names '(" *transient*")))

(provide 'init-zoom)
