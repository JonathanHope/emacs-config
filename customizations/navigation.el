(require 'helm)
(require 'helm-config)
(helm-mode 1)
(set-face-attribute 'helm-selection nil :underline 'unspecified)
(defadvice helm-display-mode-line (after undisplay-header activate)
  (setq header-line-format nil))

;; Enable projectile!
(projectile-global-mode)
(setq projectile-completion-system 'helm)
(helm-projectile-on)

;; Smex for fuzzy matching of commands.
(setq smex-save-file (concat user-emacs-directory ".smex-items"))
(smex-initialize)