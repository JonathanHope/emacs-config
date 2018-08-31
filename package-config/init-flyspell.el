;; Package configuration for flyspell.

(use-package flyspell
  :defer t

  :init
  (add-hook 'flyspell-mode-hook 'flyspell-buffer)

  :config
  (setq ispell-program-name "hunspell")
  (setq ispell-local-dictionary "en_US")
  (setq ispell-local-dictionary-alist
        '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US") nil utf-8))))

(provide 'init-flyspell)
