(use-package flycheck-languagetool
  :straight t
  :defer t
  :init
  (setq flycheck-languagetool-server-jar "~/.languagetool/languagetool-server.jar")
  :custom
  (flycheck-display-errors-delay .1))

(provide 'init-flycheck-languagetool)
