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

  :config
  (put-clojure-indent 'as-> nil)
  (put-clojure-indent 'attempt-all 1)
  (put 'defaction 'clojure-doc-string-elt 2)

  (require 'clojure-mode-extra-font-locking)

  (defvar clojure-vars '("facts" "fact" "ok->>" "ok->" "fail" "failed?" "attempt-all" "try*"))

  (font-lock-add-keywords 'clojure-mode
                          `((,(concat "(\\(?:\.*/\\)?"
                                      (regexp-opt clojure-vars t)
                                      "\\>")
                             1 font-lock-builtin-face))))

(provide 'init-clojure-mode)
