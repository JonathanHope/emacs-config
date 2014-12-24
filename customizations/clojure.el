;; Use paredit in clojure mode.
(add-hook 'clojure-mode-hook 'enable-paredit-mode)

;; Even more syntax highlighting in clojure mode.
(require 'clojure-mode-extra-font-locking)

;; Enable rainbow delimeters in clojure mode.
(add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)

;; Syntax hilighting for midje
(add-hook 'clojure-mode-hook
  (lambda ()
   (setq inferior-lisp-program "lein repl")
   (font-lock-add-keywords
   nil
   '(("(\\(facts?\\)"
     (1 font-lock-keyword-face))
     ("(\\(background?\\)"
     (1 font-lock-keyword-face))))
    (define-clojure-indent (fact 1))
    (define-clojure-indent (facts 1))))

;; Shortcut to start cider.
(global-set-key [f5] 'cider-jack-in)

;; Minibuffer documentation for cider.
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)

;; When there's a cider error, show its buffer and switch to it
(setq cider-show-error-buffer t)
(setq cider-auto-select-error-buffer t)

;; Where to store the cider history.
(setq cider-repl-history-file "~/.emacs.d/cider-history")

;; Wrap when navigating history.
(setq cider-repl-wrap-history t)
;; enable paredit in your REPL
(add-hook 'cider-repl-mode-hook 'paredit-mode)

;; Company mode autocomplete.
(add-hook 'cider-repl-mode-hook 'company-mode)
(add-hook 'cider-mode-hook 'company-mode)