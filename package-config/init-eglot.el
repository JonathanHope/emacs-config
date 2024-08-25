(use-package "eglot"
  :defer t
  
  :config
  (add-to-list 'eglot-server-programs '(elixir-ts-mode "elixir-ls"))
  (add-to-list 'eglot-server-programs '(heex-ts-mode "elixir-ls")))

(provide 'init-eglot)
