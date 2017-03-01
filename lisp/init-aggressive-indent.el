(use-package aggressive-indent
  :ensure t
  :diminish aggressive-indent-mode

  :config
  (add-hook 'clojure-mode-hook 'aggressive-indent-mode)
  (add-hook 'emacs-lisp-mode-hook 'aggressive-indent-mode)
  (add-hook 'js-mode-hook 'aggressive-indent-mode)
  (add-hook 'css-mode-hook 'aggressive-indent-mode)
  (add-hook 'csharp-mode-hook 'aggressive-indent-mode)
  (add-hook 'sgml-mode-hook 'aggressive-indent-mode))

(provide 'init-aggressive-indent)
