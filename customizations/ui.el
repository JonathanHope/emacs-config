;; Hide menu and toobar.
(menu-bar-mode -1)
(tool-bar-mode -1)

;; Disable redundant scroll bars.
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

;; Set font
(set-default-font "Envy Code R-10")

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

;; Highlight the current line
(global-hl-line-mode 1)

;; Get the current project.
(defvar m/projectile-mode-line
  '(:propertize
    (:eval (if (ignore-errors (projectile-project-root))
    (concat (projectile-project-name) "/")))))
(put 'm/custom-projectile-mode-line 'risky-local-variable t)

;; Get the current branch.
(defvar m/vc-mode-line
  '((:propertize
    (:eval (let ((backend (symbol-name (vc-backend (buffer-file-name)))))
    (concat " - " (substring vc-mode (+ (length backend) 2))))))))
(put 'm/vc-mode-line 'risky-local-variable t)

;; The name of the file being edited.
(defvar m/filename-mode-line
  '(:eval (propertize "%b" 'help-echo (buffer-file-name))))
(put 'm/filename-mode-line 'risky-local-variable t)

;; The status of the file being edited.
(defvar m/file-status-mode-line
  '(:eval
  (if (buffer-modified-p)
    "● "
  (if buffer-read-only
    "! "))))
(put 'm/file-status-mode-line 'risky-local-variable t)

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
  "═══╣ "
  " "
  m/filename-mode-line
  " ╠"

  ;; Display the row and column being edited.
  "═══╣ "
  m/row-column-mode-line
  " ╠"

  ;; Display the current major mode
  "═══╣ " 
  m/major-mode-mode-line
  " ╠"

  ;; Fill in the end of the modeline
  '(:eval (make-string 400 ?═))

  ))

(custom-set-variables
 '(initial-frame-alist (quote ((fullscreen . maximized)))))