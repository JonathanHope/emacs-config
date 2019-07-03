(use-package flyspell-correct-ivy
  :defer t
  :straight t
  :after flyspell

  :bind
  (:map flyspell-mode-map
        ("C-;" . flyspell-correct-word-generic)))

(provide 'init-flyspell-correct)
