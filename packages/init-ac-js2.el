;; Package configuration for ac-js2-mode.

(use-package ac-js2
  :ensure t
  :defer t

  :init
  (add-hook 'js2-mode-hook 'ac-js2-mode)

  :config
  (setq ac-js2-evaluate-calls t))

(provide 'init-ac-js2)
