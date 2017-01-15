;; Package configuration for helm-flyspell.

(use-package helm-flyspell
  :ensure t
  :defer t
  
  :bind
  (:map flyspell-mode-map
        ("C-;" . helm-flyspell-correct)))

(provide 'init-helm-flyspell)
