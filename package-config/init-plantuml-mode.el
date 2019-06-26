(use-package plantuml-mode
  :ensure t
  :defer t

  :init
  (setq plantuml-default-exec-mode 'jar)
  (setq plantuml-jar-path "~/.emacs.d/jar/plantuml.jar"))

(provide 'init-plantuml-mode)
