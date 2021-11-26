(use-package flyspell-correct
  :defer t
  :straight t
  :after flyspell

  :bind
  (:map flyspell-mode-map
        ("C-;" . flyspell-correct-at-point)))

(provide 'init-flyspell-correct)
