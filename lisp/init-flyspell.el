;; Package configuration for helm-flyspell.

(use-package flyspell  
  :bind
  (:map flyspell-mode-map
        ("C-;" . helm-flyspell-correct))

  :init
  (add-hook 'flyspell-mode-hook 'flyspell-buffer))

(provide 'init-flyspell)
