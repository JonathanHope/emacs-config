;; Disable the ~ files when editing.
(setq create-lockfiles nil)

;; Prevent splash screen.
(setq inhibit-splash-screen t)

;; No auto save.
(setq auto-save-default nil)

;; y and n instead of yes and no.
(fset 'yes-or-no-p 'y-or-n-p)

;; No backupt files.
(setq make-backup-files nil)
