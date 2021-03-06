(use-package rainbow-delimiters
  :defer t
  :straight t

  :init
  (add-hook 'js-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'plantuml-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'octave-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'elixir-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'perl-mode-hook #'rainbow-delimiters-mode))

(provide 'init-rainbow-delimiters)
