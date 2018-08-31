;; Package configuration magit.

(use-package magit
  :ensure t
  :defer t

  :bind
  (:map with-editor-mode-map
        ("M-c" . with-editor-finish))

  :config
  (if (eq system-type 'windows-nt)
      (progn
        (setq exec-path (add-to-list 'exec-path "C:/Program Files/Git/bin"))
        (setenv "PATH" (concat "C:\\Program Files\\Git\\bin;" (getenv "PATH"))))))

(provide 'init-magit)
