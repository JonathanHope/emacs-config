;; Package configuration for counsel.

(use-package counsel
  :ensure t

  :config
  (setq counsel-ag-base-command "ag --vimgrep --nocolor --nogroup %s"))

(provide 'init-counsel)
