(use-package clojure-mode-extra-font-locking
  :ensure t
  :defer t)

(use-package clojure-mode
  :ensure t
  :defer t
  :mode (("\\.clj$" . clojure-mode))
  
  :bind
  (:map clojure-mode-map
        ("C-<tab>" . clojure-hydra/body)
        ("<return>". newline-and-indent))

  :config
  
  ;; Even more syntax highlighting in clojure mode.
  (require 'clojure-mode-extra-font-locking))

(provide 'init-clojure-mode)
