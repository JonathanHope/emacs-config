;; Package configuration for yasnippet

(use-package yasnippet
  :ensure t
  :commands (yas-minor-mode)

  :init
  (progn
    (add-hook 'c++-mode-hook 'yas-minor-mode))

  :config
  (progn
    (yas-reload-all)))

(provide 'init-yasnippet)
