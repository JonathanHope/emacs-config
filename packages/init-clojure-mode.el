(use-package clojure-mode-extra-font-locking
  :ensure t)

(use-package clojure-mode
	:ensure t

  :bind 
  (:map clojure-mode-map
    ("C-S-<right>" . forward-sexp)
    ("C-S-<left>" . backward-sexp)
    ("C-S-k" . kill-sexp)
    ("C-S-d" . delete-sexp))

  :config
  ;; Paredit for clojure.
  (add-hook 'clojure-mode-hook 'enable-paredit-mode)

  ;; Even more syntax highlighting in clojure mode.
  (require 'clojure-mode-extra-font-locking)

  ;; Enable rainbow delimeters in clojure mode.
  (add-hook 'clojure-mode-hook #'rainbow-delimiters-mode))

(provide 'init-clojure-mode)