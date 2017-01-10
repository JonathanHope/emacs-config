;; Package configuration for isearch.

(use-package "isearch"
  :bind 
  (:map isearch-mode-map
    ("<return>" . isearch-repeat-forward)
    ("S-<return>" . isearch-repeat-backward)))

(provide 'init-isearch)