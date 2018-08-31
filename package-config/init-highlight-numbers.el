;; Package configuration for highlight numbers.

(use-package highlight-numbers
  :ensure t
  :defer t

  :init
  (add-hook 'c++-mode-hook #'highlight-numbers-mode)
  (add-hook 'js-mode-hook #'highlight-numbers-mode)
  (add-hook 'emacs-lisp-mode-hook #'highlight-numbers-mode)
  (add-hook 'rust-mode-hook #'highlight-numbers-mode)
  (add-hook 'clojure-mode-hook #'highlight-numbers-mode))

(provide 'init-highlight-numbers)
