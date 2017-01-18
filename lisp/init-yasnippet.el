;; Package configuration for yasnippet

(use-package yasnippet
  :ensure t
  :commands (yas-minor-mode)
  
  :init
  (progn
    (add-hook 'prog-mode-hook #'yas-minor-mode)
    (add-hook 'web-mode-hook #'yas-minor-mode))

  :config
  (progn
    (yas-reload-all)))

(provide 'init-yasnippet)
