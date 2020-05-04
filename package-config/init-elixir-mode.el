(use-package elixir-mode
  :straight t
  :defer t

  :mode (("\\.ex$" . elixir-mode))
  :bind
  (:map elixir-mode-map
        ("C-<tab>" . mainspring-hydra-elixir/body)
        ("<return>". newline-and-indent))

  :init
  (add-hook 'elixir-mode-hook
            (lambda () (add-hook 'before-save-hook 'elixir-format nil t))))

(provide 'init-elixir-mode)
