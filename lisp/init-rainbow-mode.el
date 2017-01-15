;; Package configuration for rainbow-mode.

(use-package rainbow-mode
  :ensure t
  :defer t

  :init
  (add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'clojure-mode-hook #'rainbow-delimiters-mode))

(provide 'init-rainbow-mode)
