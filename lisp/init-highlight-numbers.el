;; Package configuration for highlight numbers.

(use-package highlight-numbers
  :ensure t
  :defer t

  :init
  (add-hook 'prog-mode-hook #'highlight-numbers-mode))

(provide 'init-highlight-numbers)
