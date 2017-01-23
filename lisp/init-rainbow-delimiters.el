(use-package rainbow-delimiters
  :ensure t
  :defer t

  :init
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(provide 'init-rainbow-delimiters)
