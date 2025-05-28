;; -*- lexical-binding: t; -*-
(use-package "eglot"
  :defer t

  :config
  (setq eglot-code-action-indications '())
  
  :hook ((typescript-ts-mode . eglot-ensure)
         (tsx-ts-mode . eglot-ensure)
         (js-ts-mode . eglot-ensure)
         (go-ts-mode . eglot-ensure)))

(provide 'init-eglot)
