(use-package plantuml-mode
  :defer t
  :straight t

  :init
  (setq plantuml-default-exec-mode 'jar)
  (setq plantuml-jar-path "~/.emacs.d/jar/plantuml.jar"))

(provide 'init-plantuml-mode)
