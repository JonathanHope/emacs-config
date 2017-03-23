;; Package configuration for flyspell.

(use-package flyspell
  :defer t

  :init
  (add-hook 'flyspell-mode-hook 'flyspell-buffer))

(provide 'init-flyspell)
