;; Package configuration for aggressive-indent.

(use-package aggressive-indent
  :ensure t
  :defer t
  :diminish aggressive-indent-mode

  :init
  (add-hook 'clojure-mode-hook 'aggressive-indent-mode)
  (add-hook 'emacs-lisp-mode-hook 'aggressive-indent-mode)
  (add-hook 'js-mode-hook 'aggressive-indent-mode)
  (add-hook 'css-mode-hook 'aggressive-indent-mode)
  (add-hook 'csharp-mode-hook 'aggressive-indent-mode)
  (add-hook 'sgml-mode-hook 'aggressive-indent-mode)
  (add-hook 'c++-mode-hook 'aggressive-indent-mode))

(provide 'init-aggressive-indent)
