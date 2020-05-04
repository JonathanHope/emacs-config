(use-package expand-region
  :defer t
  :straight t

  :init
  (require 'ruby-mode-expansions)
  (er/enable-mode-expansions 'elixir-mode 'er/add-ruby-mode-expansions))

(provide 'init-expand-region)
