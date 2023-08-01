(use-package dockerfile-mode
  :defer t
  :straight t

  :mode (("Dockerfile\\(?:\\..*\\)?\\'" . dockerfile-ts-mode)))

(provide 'init-dockerfile-mode)
