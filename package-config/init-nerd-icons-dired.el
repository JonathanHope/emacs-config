;; -*- lexical-binding: t; -*-
(use-package nerd-icons-dired
  :straight t
  :defer t
  
  :hook
  (dired-mode . nerd-icons-dired-mode)

  :init
  (advice-add 'dired-subtree-toggle :after (lambda ()
                                             (interactive)
                                             (when nerd-icons-dired-mode
                                               (revert-buffer)))))

(provide 'init-nerd-icons-dired)
