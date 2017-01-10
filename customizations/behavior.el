;; Configuration of Emacs behavior.

;; No tabs, use spaces instead.
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)

;; Highlight matching parens.
(show-paren-mode 1)

;; Allow up and down casing regions.
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; Allow sections to be deleted.
(delete-selection-mode 1)

;; Disable the ~ files when editing.
(setq create-lockfiles nil)

;; No auto save.
(setq auto-save-default nil)

;; No backup files.
(setq make-backup-files nil)