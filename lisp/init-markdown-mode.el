;; Package configuration for markdown-mode.

(use-package markdown-mode
  :ensure t
  :defer t

  :mode
  (("README\\.md\\'" . gfm-mode)
   ("\\.md\\'" . markdown-mode)
   ("\\.markdown\\'" . markdown-mode))

  :bind
  (:map markdown-mode-map
        ("C-<tab>" . markdown-hydra/body)
        :map gfm-mode-map
        ("C-<tab>" . markdown-hydra/body)))

(provide 'init-markdown-mode)
