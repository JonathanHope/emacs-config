(use-package rainbow-delimiters
  :defer t
  :straight t

  :init
  (add-hook 'js-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'octave-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'typescript-ts-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'tsx-ts-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'js-ts-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'go-ts-mode-hook #'rainbow-delimiters-mode))

(provide 'init-rainbow-delimiters)
