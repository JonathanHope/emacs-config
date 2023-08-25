(use-package apheleia
  :straight t
  :defer t

  :hook ((typescript-ts-mode . apheleia-mode)
         (tsx-ts-mode . apheleia-mode)
         (js-ts-mode . apheleia-mode)
         (go-ts-mode . apheleia-mode)))

(provide 'init-apheleia)
