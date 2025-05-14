(use-package consult
  :straight t

  :init
  (setq consult-project-root-function #'projectile-project-root)

  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  (advice-add #'register-preview :override #'consult-register-window)

  :config
  (consult-customize consult-ripgrep :preview-key nil))

(provide 'init-consult)
