(use-package aggressive-indent
  :ensure t
  :defer t

  :init
  (add-hook 'sgml-mode-hook 'aggressive-indent-mode)
  (add-hook 'js-mode-hook 'aggressive-indent-mode)
  (add-hook 'emacs-lisp-mode-hook 'aggressive-indent-mode)
  (add-hook 'clojure-mode-hook 'aggressive-indent-mode)
  (add-hook 'plantuml-mode-hook 'aggressive-indent-mode))

(provide 'init-aggressive-indent)
