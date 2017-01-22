(use-package ws-butler
  :ensure t
  :diminish ws-butler-mode
  :config
  (add-hook 'prog-mode-hook 'ws-butler-mode)
  (add-hook 'sgml-mode-hook 'ws-butler-mode)
  (add-hook 'org-mode-hook 'ws-butler-mode))

(provide 'init-ws-butler)
