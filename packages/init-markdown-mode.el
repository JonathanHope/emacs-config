(use-package markdown-mode
  :ensure t

  :commands (markdown-mode gfm-mode)

  :mode 
  (("README\\.md\\'" . gfm-mode)
   ("\\.md\\'" . markdown-mode)
   ("\\.markdown\\'" . markdown-mode))

  :bind
  (:map markdown-mode-map
        ("C-<tab>" . markdown-hydra/body)
   :map gfm-mode-map
        ("C-<tab>" . markdown-hydra/body))

  :init 
  (setq markdown-command "multimarkdown"))

(provide 'init-markdown-mode)
