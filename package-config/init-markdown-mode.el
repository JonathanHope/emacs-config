;; -*- lexical-binding: t; -*-
(use-package markdown-mode
  :defer t
  :straight t

  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))

  :bind
  (:map markdown-mode-map
        ("C-<tab>" . mainspring-hydra-markdown/body))

  :config
  (setq markdown-command "multimarkdown")
  (setq markdown-split-window-direction 'right))

(provide 'init-markdown-mode)
