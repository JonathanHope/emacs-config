(use-package ebnf-mode
  :defer t
  :straight (:type git
                   :host github
                   :repo "jeramey/ebnf-mode"
                   :branch "master"
                   :fork (:host github
                                :repo "JonathanHope/ebnf-mode"))

  :mode (("\\.bnf$" . ebnf-mode)
         ("\\.ebnf$" . ebnf-mode)))

(provide 'init-ebnf-mode)
