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
  (require 'clojure-mode-extra-font-locking))

(provide 'init-clojure-mode)
