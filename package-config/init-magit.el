;; Package configuration magit.

(use-package magit
  :ensure t
  :defer t

  :bind
  (:map with-editor-mode-map ("M-c" . with-editor-finish))
  (:map transient-base-map ("q" . transient-quit-one))
  (:map transient-base-map ("<escape>" . transient-quit-one))
  (:map transient-edit-map ("q" . transient-quit-one))
  (:map transient-edit-map ("<escape>" . transient-quit-one))
  (:map transient-sticky-map ("q" . transient-quit-one))
  (:map transient-sticky-map ("<escape>" . transient-quit-one))

  :config
  ;; (setq magit-display-buffer-function
  ;;       (lambda (buffer)
  ;;         (display-buffer buffer '(display-buffer-same-window))))
  (setq transient-save-history nil)
  (if (eq system-type 'windows-nt)
      (progn
        (setq exec-path (add-to-list 'exec-path "C:/Program Files/Git/bin"))
        (setenv "PATH" (concat "C:\\Program Files\\Git\\bin;" (getenv "PATH"))))))

(provide 'init-magit)
