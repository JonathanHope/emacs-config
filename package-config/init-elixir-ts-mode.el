(use-package "elixir-ts-mode"
  :defer t

  :mode (("\\.ex$" . elixir-ts-mode)
         ("\\.exs$" . elixir-ts-mode))

  :hook
  (elixir-ts-mode . eglot-ensure))

(provide 'init-elixir-ts-mode)
