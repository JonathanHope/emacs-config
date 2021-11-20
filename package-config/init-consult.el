(use-package consult
  :straight t

  :init
  (setq consult-project-root-function #'projectile-project-root))

(provide 'init-consult)
