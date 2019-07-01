(use-package magit
  :ensure t
  :defer t

  :bind
  (:map with-editor-mode-map ("M-c" . with-editor-finish))
  (:map with-editor-mode-map ("S-<escape>" . with-editor-cancel))
  (:map transient-base-map ("q" . transient-quit-one))
  (:map transient-base-map ("<escape>" . transient-quit-one))
  (:map transient-edit-map ("q" . transient-quit-one))
  (:map transient-edit-map ("<escape>" . transient-quit-one))
  (:map transient-sticky-map ("q" . transient-quit-one))
  (:map transient-sticky-map ("<escape>" . transient-quit-one))

  :init
  (setq magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1)
  (setq transient-save-history nil)

  :config
  (if (eq system-type 'windows-nt)
      (progn
        (setq exec-path (add-to-list 'exec-path "C:/Program Files/Git/bin"))
        (setenv "PATH" (concat "C:\\Program Files\\Git\\bin;" (getenv "PATH"))))))

(provide 'init-magit)
