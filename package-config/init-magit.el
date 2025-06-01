;; -*- lexical-binding: t; -*-

;; TODO: there seems to be a bug with the recipe; I should be able to remove this.
(use-package llama
  :defer t
  :straight (llama :type git :host github :repo "tarsius/llama"))

;; TODO: this should be a part of magit; I should be able to remove it.
(use-package git-commit
  :defer t
  :straight t)

(use-package transient
  :defer t
  :straight t

  :bind
  (:map transient-base-map ("q" . transient-quit-one))
  (:map transient-base-map ("<escape>" . transient-quit-one))
  (:map transient-edit-map ("q" . transient-quit-one))
  (:map transient-edit-map ("<escape>" . transient-quit-one))
  (:map transient-sticky-map ("q" . transient-quit-one))
  (:map transient-sticky-map ("<escape>" . transient-quit-one)))

(use-package magit
  :defer t
  :straight t
  :after llama

  :bind
  (:map with-editor-mode-map ("M-c" . with-editor-finish))
  (:map with-editor-mode-map ("S-<escape>" . with-editor-cancel))

  :init
  (setq magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1)
  (setq transient-save-history nil)

  :config
  (if (eq system-type 'windows-nt)
      (progn
        (setq exec-path (add-to-list 'exec-path "C:/Program Files/Git/bin"))
        (setenv "PATH" (concat "C:\\Program Files\\Git\\bin;" (getenv "PATH"))))))

(provide 'init-magit)
