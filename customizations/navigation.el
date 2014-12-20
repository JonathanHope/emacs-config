;; Ido for fuzzy matching
(ido-mode t)
(setq ido-enable-flex-matching t)
(setq ido-use-filename-at-point nil)
(setq ido-auto-merge-work-directories-length -1)
(setq ido-use-virtual-buffers t)
(ido-ubiquitous-mode 1)

;; Enable projectile!
(projectile-global-mode)

;; Smex for fuzzy matching of commands.
(setq smex-save-file (concat user-emacs-directory ".smex-items"))
(smex-initialize)