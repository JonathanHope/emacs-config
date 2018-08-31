;; Package configuration for rainbow delimiters.

(use-package rainbow-delimiters
  :ensure t
  :defer t

  :init
  (add-hook 'c++-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'js-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'rust-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'clojure-mode-hook #'rainbow-delimiters-mode))

(provide 'init-rainbow-delimiters)
