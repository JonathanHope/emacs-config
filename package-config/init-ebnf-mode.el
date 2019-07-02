(use-package ebnf-mode
  :defer t
  :straight t

  :mode (("\\.bnf$" . ebnf-mode)
         ("\\.ebnf$" . ebnf-mode)))

(provide 'init-ebnf-mode)
