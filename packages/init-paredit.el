; Package configuration for paredit.

(use-package paredit
  :ensure t
  :defer t

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

  :config
  (defun paredit-inside-sexp-p ()
    "Are we inside the bounds of a sexp?"
    (= (save-excursion (paredit-forward)
                       (point))
       (save-excursion (paredit-backward)
                       (paredit-forward)
                       (point))))

  (defun paredit-start-of-sexp-p ()
    "Are we at the start of a sexp?"
    (= (save-excursion (paredit-forward)
                       (paredit-backward)
                       (point))
       (point)))

  (defun delete-sexp ()
    "Delete the current sexp."
    (interactive)
    (cond
     ((paredit-in-comment-p)
      (call-interactively 'delete-char))
     ((paredit-in-string-p)
      (delete-region (save-excursion (paredit-backward-up)
                                     (point))
                     (save-excursion (paredit-backward-up)
                                     (paredit-forward)
                                     (point))))
     ((paredit-inside-sexp-p)
      (delete-region (save-excursion (paredit-backward)
                                     (point))
                     (save-excursion (paredit-forward)
                                     (point))))
     ((paredit-start-of-sexp-p)
      (delete-region (point)
                     (save-excursion (paredit-forward)
                                     (point))))
     (t
      (delete-region (save-excursion (paredit-backward)
                                     (point))
                     (save-excursion (paredit-backward)
                                     (paredit-forward)
                                     (point))))))

(provide 'init-paredit)
