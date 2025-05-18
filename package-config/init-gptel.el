;; -*- lexical-binding: t; -*-
(use-package gptel
  :straight t
  :defer t

  ;; TODO: I would like to remove the prompt when I launch it
  ;; https://github.com/karthink/gptel/issues/318
  
  :init
  (setq gptel-model 'gpt-4o) ;; mini doesn't seem to work
  (setq gptel-backend
        (gptel-make-gh-copilot "Copilot"))
  (setq gptel-default-mode 'org-mode))

(provide 'init-gptel)
