;; Package configuration for flyspell-correct.

(use-package flyspell-correct-ivy
  :ensure t
  :defer t
  :after flyspell

  :bind
  (:map flyspell-mode-map
        ("C-;" . flyspell-correct-word-generic)))

(provide 'init-flyspell-correct)
