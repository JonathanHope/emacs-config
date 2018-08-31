;; Package configuration for aggressive-indent.

(use-package aggressive-indent
  :ensure t
  :defer t
  :diminish aggressive-indent-mode

  :init
  (add-hook 'c++-mode-hook 'aggressive-indent-mode)
  (add-hook 'sgml-mode-hook 'aggressive-indent-mode)
  (add-hook 'js-mode-hook 'aggressive-indent-mode)
  (add-hook 'emacs-lisp-mode-hook 'aggressive-indent-mode)
  (add-hook 'rust-mode-hook 'aggressive-indent-mode)
  (add-hook 'clojure-mode-hook 'aggressive-indent-mode))

(provide 'init-aggressive-indent)
