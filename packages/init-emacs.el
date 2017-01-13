;; Package configuration for emacs in general.

(use-package "emacs"
  :bind
  (:map emacs-lisp-mode-map
    ("C-<tab>" . elisp-hydra/body))
  
  :init
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

  ;; Hide menu, toolbar, and fringe.
  (menu-bar-mode 0)
  (tool-bar-mode 0)
  (set-fringe-mode 0)

  ;; Hide the initial scratch menu.
  (setq initial-scratch-message "")

  ;; Disable the scrollbar.
  (when (fboundp 'scroll-bar-mode)
    (scroll-bar-mode -1))

  ;; Set font
  (defvar Input-font '(:family "Input" :size 14))
  (defvar PragmataPro-font '(:family "PragmataPro" :size 14))
  (set-frame-font (apply 'font-spec PragmataPro-font) nil t)

  ;; Disable bell
  (setq ring-bell-function 'ignore)

  ;; Scroll one line at a time.
  (setq redisplay-dont-pause t
    scroll-margin 1
    scroll-step 1
    scroll-conservatively 10000
    scroll-preserve-screen-position 1)

  ;; Set cursor type.
  (setq-default cursor-type 'bar)

  ;; Set color scheme.
  (add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
  (add-to-list 'load-path "~/.emacs.d/themes")
  (load-theme 'base16-ocean-dark t)

  (set-face-background 'vertical-border "#343D46")
  (set-face-foreground 'vertical-border (face-background 'vertical-border))

  ;; Highlight the current line
  (global-hl-line-mode 1)

  ;; The name of the file being edited.
  (defvar m/filename-mode-line
    '(:eval (propertize "%b" 'help-echo (buffer-file-name))))
  (put 'm/filename-mode-line 'risky-local-variable t)

  ;; The current row and column being edited.
  (defvar m/row-column-mode-line
    (concat (propertize "%01l") "," (propertize "%01c")))
  (put 'm/row-column-mode-line 'risky-local-variable t)

  ;; The current major mode.
  (defvar m/major-mode-mode-line
    '(:eval mode-name))
  (put 'm/major-mode-mode-line 'risky-local-variable t)

  ;; Custom mode line.
  (setq-default mode-line-format
    (list

    ;; Is the buffer modified.
    "━━━⬢ "
    " "
    m/filename-mode-line
    " ⬢"

    ;; Display the row and column being edited.
    "━━━⬢ "
    m/row-column-mode-line
    " ⬢"

    ;; Display the current major mode
    "━━━⬢ " 
    m/major-mode-mode-line
    " ⬢"

    ;; Fill in the end of the modeline
    '(:eval (make-string 400 ?━))

    ))

  ;; Start in full screen.
  (custom-set-variables
   '(initial-frame-alist (quote ((fullscreen . maximized)))))

  ;; Prevent splash screen.
  (setq inhibit-splash-screen t)

  ;; y and n instead of yes and no.
  (fset 'yes-or-no-p 'y-or-n-p)

  ;; Hide initial minibuffer text.
  (defun display-startup-echo-area-message ()
    (message ""))

  ;; Enable pretty utf8 char replacements.
  (global-prettify-symbols-mode +1)
  ;; TODO: Seems buggy.
  ;;(setq prettify-symbols-unprettify-at-point 'right-edge)
  
  (defun revert-default-directory ()
    "Revert the default directory to the directory that emacs was started in."
    (setq default-directory old-default-directory))

  (defun startup ()
    "Custom startup function. Defaults to org mode in the notes directory."
    (make-directory notes-directory :parents)
    (make-directory scratch-directory :parents)

    (let ((projectile-file (concat notes-directory ".projectile")))
      (if (not (file-exists-p projectile-file))
          (write-region "-/Scratch/node_modules" nil projectile-file)
        nil))

    (dolist (scratch-file scratch-files) 
      (let ((scratch-file-path (concat scratch-directory scratch-file)))
        (if (not (file-exists-p scratch-file-path))
            (write-region "" nil scratch-file-path)
          nil)))
    
    (setq default-directory notes-directory)
    (org-mode)))

(provide 'init-emacs)
