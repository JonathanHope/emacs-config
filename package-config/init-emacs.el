;; -*- lexical-binding: t; -*-
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
  (load-theme 'mainspring t)

  ;; Hide vertical boder
  (set-face-attribute 'vertical-border
                      nil
                      :foreground "#2B303B")

  ;; Set the default font.
  (set-face-attribute 'default nil
                      :family "PragmataPro Mono"
                      :height 100)

  ;; Set the modeline font.
  (let ((faces '(mode-line
                 mode-line-inactive
                 mainspring-mode-line-face
                 mainspring-mode-line-inactive-face
                 mainspring-mode-line-window-number-face
                 mainspring-mode-line-file-status-face
                 mainspring-mode-line-buffer-name-face
                 mainspring-mode-line-projectile-face
                 mainspring-mode-line-mode-face
                 mainspring-mode-line-row-column-face
                 mainspring-mode-line-scroll-bar-face)))
    (mapc
     (lambda (face) (set-face-attribute face nil
                                        :family "PragmataPro"
                                        :height 100))
     faces))

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

  ;; No newlines at end of file.
  (setq require-final-newline nil)

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

  ;; Don't prompt to kill process buffers.
  (setq kill-buffer-query-functions nil)

  ;; No popup modals.
  (setq use-dialog-box nil)

  ;; Most tree-sitter highlighting.
  (setq treesit-font-lock-level 4)

  ;; Hide ^M characters.
  (add-hook 'after-change-major-mode-hook 'hide-dos-eol)

  ;; Support opening new minibuffers from inside existing minibuffers.
  (setq enable-recursive-minibuffers t)

  ;; Do not allow the cursor in the minibuffer prompt.
  (setq minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt))

  ;; Hide commands in M-x which do not work in the current mode.
  (setq read-extended-command-predicate #'command-completion-default-include-p)

  ;; More efficient garbage collection.
  (setq gc-cons-threshold #x40000000)
  (defvar k-gc-timer
  (run-with-idle-timer 15 t
                       (lambda ()
                         (garbage-collect))))

  (defun hide-dos-eol ()
    "Hide ^M in files containing mixed UNIX and DOS line endings."
    (interactive)
    (setq buffer-display-table (make-display-table))
    (aset buffer-display-table ?\^M []))

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
      (if face (message "Face: %s" face) (message "No face at %d" pos))))

  ;; Register any languages with treesitter support here.
  (setq treesit-language-source-alist
   '((typescript "https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.2" "typescript/src")
     (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.2" "tsx/src")
     (json "https://github.com/tree-sitter/tree-sitter-json")
     (yaml "https://github.com/ikatyang/tree-sitter-yaml")
     (css "https://github.com/tree-sitter/tree-sitter-css")
     (html "https://github.com/tree-sitter/tree-sitter-html")
     (javascript "https://github.com/tree-sitter/tree-sitter-javascript")
     (mermaid "https://github.com/monaqa/tree-sitter-mermaid")
     (go "https://github.com/tree-sitter/tree-sitter-go" "v0.20.0")
     (gomod "https://github.com/camdencheek/tree-sitter-go-mod")
     (cmake "https://github.com/uyha/tree-sitter-cmake" "v0.2.0")
     (bash "https://github.com/tree-sitter/tree-sitter-bash")
     (toml "https://github.com/tree-sitter/tree-sitter-toml")
     (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")))
  
  (defun treesit-install-all ()
    (interactive)
    (mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist))))

(provide 'init-emacs)
