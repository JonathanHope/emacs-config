;; -*- lexical-binding: t; -*-
(use-package copilot
  :straight (:host github :repo "copilot-emacs/copilot.el" :files ("*.el"))
  :ensure t

  :config
  (setq copilot-idle-delay nil)
  
  :bind
  (:map copilot-mode-map
        ("<backtab>" . copilot-accept-completion)
        ("C-:" . copilot-complete))
  
  :hook ((typescript-ts-mode . copilot-mode)
         (tsx-ts-mode . copilot-mode)
         (js-ts-mode . copilot-mode)
         (go-ts-mode . copilot-mode)))

(provide 'init-copilot)
