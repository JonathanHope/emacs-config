(use-package clojure-mode-extra-font-locking
  :straight t
  :defer t)

(use-package clojure-mode
  :straight t
  :defer t

  :mode (("\\.clj$" . clojure-mode)
         ("\\.cljs$" . clojurescript-mode)
         ("\\.boot$" . clojure-mode))
  :bind
  (:map clojure-mode-map
        ("C-<tab>" . mainspring-hydra-clojure/body)
        ("<return>". newline-and-indent))

  :init

  (defvar mainspring-clojure-prettify-alist '())

  (add-to-list 'mainspring-clojure-prettify-alist
               '("<=" . ?≤))
  (add-to-list 'mainspring-clojure-prettify-alist
               '(">=" . ?≥))
  (add-to-list 'mainspring-clojure-prettify-alist
               '("=>" . ?⇒))
  (add-to-list 'mainspring-clojure-prettify-alist
               '("->" . (?- (Br . Bc) ?- (Br . Bc) ?>)))
  (add-to-list 'mainspring-clojure-prettify-alist
               '("->>" .  (?\s (Br . Bl) ?\s (Br . Bl) ?\s
                               (Bl . Bl) ?- (Bc . Br) ?- (Bc . Bc) ?>
                               (Bc . Bl) ?- (Br . Br) ?>)))

  (eval-after-load 'clojure-mode
    '(setq clojure--prettify-symbols-alist
           (append mainspring-clojure-prettify-alist
                   clojure--prettify-symbols-alist)))

  :config
  (require 'clojure-mode-extra-font-locking)

  (defvar clojure-midje-vars '("facts" "fact"))

  (font-lock-add-keywords 'clojure-mode
                          `((,(concat "(\\(?:\.*/\\)?"
                                      (regexp-opt clojure-midje-vars t)
                                      "\\>")
                             1 font-lock-builtin-face))))

(provide 'init-clojure-mode)
