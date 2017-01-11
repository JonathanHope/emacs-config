; Package configuration for paredit.

(use-package paredit
  :ensure t

  :bind
  (:map  paredit-mode-map
    ("M-<up>" . nil)
    ("M-<down>" . nil)
    ("C-S-<right>" . forward-sexp)
    ("C-S-<left>" . backward-sexp)
    ("C-S-k" . kill-sexp)
    ("C-S-d" . delete-sexp))

  :init
  (add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
  (add-hook 'clojure-mode-hook 'enable-paredit-mode)
  (add-hook 'cider-repl-mode-hook 'enable-paredit-mode))

(provide 'init-paredit)
