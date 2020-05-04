(use-package mix
  :defer t
  :straight (:type git :host github :repo "ayrat555/mix.el" :branch "master" :files ("mix.el"))


  :init
  (add-hook 'elixir-mode-hook 'mix-minor-mode))

(provide 'init-mix)
