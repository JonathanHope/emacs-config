;; Package configuration for deft.

(use-package deft
  :ensure t

  :commands (deft)

  :bind
  (:map deft-mode-map
        ("C-<tab>" . deft-hydra/body))

  :config
  (setq deft-extensions '("org"))
  (setq deft-directory "~/Notes")
  (setq deft-archive-directory "Archive/")
  (setq deft-file-limit 20 )
  (setq deft-filter-only-filenames t)
  (setq deft-use-filter-string-for-filename t)
  (setq deft-default-extension "org")
  (setq deft-strip-summary-regexp (concat 
  	"\\("
    "[\n\t]" ;; blank
    "\\|^#\\+[[:upper:]_]+:.*$" ;; org-mode metadata
    "\\|^\\*..*$" ;; org-mode headers
    "\\)"))
  (setq deft-file-naming-rules '((noslash . "-")
                                 (nospace . "-")
                                 (case-fn . downcase))))

(provide 'init-deft)