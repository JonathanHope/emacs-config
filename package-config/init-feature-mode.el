(use-package feature-mode
  :straight t
  :defer t

  :mode (("\\.feature$" . feature-mode))

  :init
  (defun turn-on-orgtbl ()
    "Clear terminal"
    (interactive)
    (orgtbl-mode))
  ;; (add-hook 'feature-mode-hook #'orgtbl-mode)
  )

(provide 'init-feature-mode)
