(use-package mermaid-ts-mode
  :straight (:type git :host github :repo "JonathanHope/mermaid-ts-mode" :branch "main" :files ("mermaid-ts-mode.el"))
  :defer t

  :mode (("\\.mmd$" . mermaid-ts-mode)))

(provide 'init-mermaid-mode)
