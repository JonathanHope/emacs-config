(use-package "emacs"
  :bind
  (:map emacs-lisp-mode-map
        ("<return>". newline-and-indent))

  :init
  ;; No tabs, use spaces instead.
  (setq-default indent-tabs-mode nil)
  (setq-default tab-width 2)

  ;; Allow up and down casing regions.
  (put 'downcase-region 'disabled nil)
  (put 'upcase-region 'disabled nil)

  ;; Allow sections to be deleted.
  (delete-selection-mode 1)

  ;; Disable the ~ files when editing.
  (setq create-lockfiles nil)

  ;; No auto save.
  (setq auto-save-default nil)
  (setq auto-save-list-file-prefix nil)

  ;; No backup files.
  (setq make-backup-files nil)

  ;; Hide menu, toolbar, and fringe.
  (menu-bar-mode 0)
  (tool-bar-mode 0)
  (fringe-mode '(0 . 0))

  ;; Hide the initial scratch menu.
  (setq initial-scratch-message "")

  ;; Disable the scrollbar.
  (when (fboundp 'scroll-bar-mode)
    (scroll-bar-mode -1))

  ;; Disable the pointless vertical bar.
  (set-display-table-slot standard-display-table
                          'vertical-border (make-glyph-code 8203))

  ;; Set font
  (defvar Input-font '(:family "Input" :size 12))
  (defvar PragmataPro-font '(:family "PragmataPro" :size 12))
  (set-frame-font (apply 'font-spec PragmataPro-font) nil t)

  ;; Disable bell
  (setq ring-bell-function 'ignore)

  ;; Scroll one line at a time.
  (setq
   redisplay-dont-pause t
   hscroll-margin 1
   hscroll-step 1
   scroll-conservatively 1001
   scroll-margin 0
   scroll-preserve-screen-position t
   auto-window-vscroll nil)

  ;; Set cursor type.
  (setq-default cursor-type 'bar)

  ;; Set color scheme.
  (add-to-list 'custom-theme-load-path "~/.emacs.d/packages")
  (load-theme 'mainspring t)

  ;; Set the border colors.
  (set-face-background 'vertical-border "#343D46")
  (set-face-foreground 'vertical-border (face-background 'vertical-border))

  ;; Highlight the current line
  (global-hl-line-mode 1)

  ;; Start in full screen.
  (add-to-list 'default-frame-alist '(fullscreen . maximized))

  ;; Prevent splash screen.
  (setq inhibit-splash-screen t)

  ;; y and n instead of yes and no.
  (fset 'yes-or-no-p 'y-or-n-p)

  ;; Don't delete prompts.
  (setq comint-prompt-read-only t)

  ;; Hide initial minibuffer text.
  (defun display-startup-echo-area-message ()
    (message ""))

  ;; Enable pretty utf8 char replacements.
  (global-prettify-symbols-mode +1)

  ;; No newlines at end of file.
  (setq require-final-newline t)

  ;; Remove trailing whitespace.
  (add-hook 'before-save-hook 'delete-trailing-whitespace)

  ;; Hide the arrows that appear in the fringe for continued lines.
  (define-fringe-bitmap 'right-curly-arrow
    [#b00000000
     #b00000000
     #b00000000
     #b00000000
     #b00000000
     #b00000000
     #b00000000
     #b00000000])
  (define-fringe-bitmap 'left-curly-arrow
    [#b00000000
     #b00000000
     #b00000000
     #b00000000
     #b00000000
     #b00000000
     #b00000000
     #b00000000])

  ;; Hide the slash that appears at the end of lines that are going to wrap.
  (set-display-table-slot standard-display-table 'wrap ?\ )

  ;; Don't show the cursor in windows that are not selected.
  (setq-default cursor-in-non-selected-windows nil)

  ;; Change the frame title to show the buffer name.
  (setq frame-title-format "%b - Mainspring")

  ;; Disable shift to select.
  (setq shift-select-mode nil)

  ;; Speed up font drawing a bit.
  (setq inhibit-compacting-font-cache t)

  ;; Prevent ad warnings.
  (setq ad-redefinition-action 'accept)

  (defun mainspring-my-delete-trailing-blank-lines ()
    "Deletes all blank lines at the end of the file."
    (interactive)
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-max))
        (delete-blank-lines))))
  (add-hook 'before-save-hook 'mainspring-my-delete-trailing-blank-lines)

  (defun mainspring-startup ()
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
    (text-mode))

  (defun revert-default-directory ()
    "Revert the default directory to the directory that emacs was started in."
    (setq default-directory old-default-directory))

  (defun what-face (pos)
    "Get the face in use at the current position."
    (interactive "d")
    (let ((face (or (get-char-property (point) 'read-face-name)
                    (get-char-property (point) 'face))))
      (if face (message "Face: %s" face) (message "No face at %d" pos)))))

(provide 'init-emacs)
