;; Package configuration for flyspell-correct.

(use-package flyspell-correct-ivy
  :ensure t
  :defer 0

  :bind
  (:map flyspell-mode-map
        ("C-;" . flyspell-correct-previous-word-generic)))

(provide 'init-flyspell-correct)
