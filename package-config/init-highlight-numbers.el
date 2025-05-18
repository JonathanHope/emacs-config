;; -*- lexical-binding: t; -*-
(use-package highlight-numbers
  :defer t
  :straight t

  :init
  (add-hook 'js-mode-hook #'highlight-numbers-mode)
  (add-hook 'emacs-lisp-mode-hook #'highlight-numbers-mode)
  (add-hook 'yaml-mode-hook #'highlight-numbers-mode)
  (add-hook 'typescript-ts-mode-hook #'highlight-numbers-mode)
  (add-hook 'tsx-ts-mode-hook #'highlight-numbers-mode)
  (add-hook 'js-ts-mode-hook #'highlight-numbers-mode)
  (add-hook 'go-ts-mode-hook #'highlight-numbers-mode))

(provide 'init-highlight-numbers)
