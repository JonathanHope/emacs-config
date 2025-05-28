;; -*- lexical-binding: t; -*-
(use-package shell-pop
  :straight t
  :defer t

  :init
  (setq shell-pop-autocd-to-working-dir t)
  (setq shell-pop-window-position "bottom")
  (setq shell-pop-full-span t)
  (setq shell-pop-shell-type '("eshell" "*eshell*" (lambda () (eshell)))))

(provide 'init-shell-pop)
