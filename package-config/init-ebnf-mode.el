;; Package configuration for ebnf mode.

(use-package ebnf-mode
  :defer t

  :mode (("\\.bnf$" . ebnf-mode)
         ("\\.ebnf$" . ebnf-mode)))

(provide 'init-ebnf-mode)
