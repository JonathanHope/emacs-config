(use-package aggressive-indent
  :ensure t
  :diminish aggressive-indent-mode

  :config
  (add-hook 'prog-mode-hook 'aggressive-indent-mode)
  (add-hook 'sgml-mode-hook 'aggressive-indent-mode))

(provide 'init-aggressive-indent)
