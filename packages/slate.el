(require 'subr-x)
(require 'widget)
(require 'wid-edit)
(require 'cl)

;; Customization

(defgroup slate nil
  "Emacs Slate mode."
  :group 'local)

(defcustom slate-directory (expand-file-name "~/Notes/")
  "Slate directory."
  :type 'directory
  :safe 'stringp
  :group 'slate)

(defcustom slate-todo-indicator "⬜"
  "What character to demark a TODO."
  :group 'slate)

;; Faces

(defgroup slate-faces nil
  "Faces used in Slate mode"
  :group 'slate
  :group 'faces)

(defface slate-header-face
  '((t :inherit font-lock-keyword-face :bold t))
  "Face for Slate header."
  :group 'slate-faces)

(defface slate-todo-indicator-face
  '((t :inherit font-lock-variable-name-face :bold t))
  "Face for Slate TODO indicator."
  :group 'slat-faces)

(defface slate-file-name-face
  '((t :inherit font-lock-function-name-face :bold t))
  "Face for Slate file name."
  :group 'slate-faces)

(defface slate-divider-face
  '((t :inherit font-lock-type-face :bold t))
  "Face for Slate file name line number divider."
  :group 'slate-faces)

(defface slate-line-number-face
  '((t :inherit font-lock-function-name-face :bold t))
  "Face for Slate line number."
  :group 'slate-faces)

(defface slate-todo-face
  '((t :inherit font-lock-string-face))
  "Face for Slate TODO."
  :group 'slate-faces)

;; Constants

(defconst slate-buffer "*Slate*"
  "Slate buffer name.")

;; Variables

(defvar slate-todos nil)

(defvar slate-max-file-name-length nil)

;; Helpers

(defun slate-zip-with (list1 list2 fn)
  (let ((r nil))
    (while (and list1 list2)
      (!cons (funcall fn (car list1) (car list2)) r)
      (!cdr list1)
      (!cdr list2))
    (nreverse r)))

(defun slate-find-max-file-name-length ()
  (let ((file-name-lengths (mapcar (lambda (elt) (+ (length (nth 0 elt))
                                               (length (number-to-string (nth 1 elt)))
                                               1)) slate-todos)))
    (setq slate-max-file-name-length (reduce #'max file-name-lengths))
    (setq slate-todos (slate-zip-with slate-todos file-name-lengths (lambda (e1 e2) (append e1 (list e2)))))))

(defun slate-find-todos ()
  (let* ((rg-output (shell-command-to-string (concat "rg -n \"^\*. TODO \" " default-directory)))
         (todo-lines (split-string rg-output "\n"))
         (todo-lines-non-empty (seq-filter (lambda (elt)
                                             (not (string= elt ""))) todo-lines))
         (todos-tokens (mapcar (lambda (elt)
                                 (split-string elt ":")) todo-lines-non-empty))
         (todos (mapcar (lambda (elt)
                          (let ((file-name (nth 0 elt))
                                (line-number (nth 1 elt))
                                (todo (nth 2 elt)))
                            (list
                             (file-name-nondirectory file-name)
                             (string-to-number line-number)
                             (string-trim (replace-regexp-in-string "^\\*. ?TODO " "" todo))))) todos-tokens)))
    (setq slate-todos todos)))

(defun slate-buffer-visible-p ()
  (get-buffer-window slate-buffer))

(defun slate-print-header ()
  (progn
    (widget-insert
     (propertize "Slate" 'face 'slate-header-face))
    (widget-insert "\n\n")))

(defun slate-todo-widget (todo)
  (let ((file-name (nth 0 todo))
        (line-number (nth 1 todo))
        (todo (nth 2 todo))
        (file-name-length (nth 3 todo)))
    (progn
      (widget-insert (propertize slate-todo-indicator 'face 'slate-todo-indicator-face))
      (widget-insert " ")
      (widget-insert (propertize file-name 'face 'slate-file-name-face))
      (widget-insert (propertize ":" 'face 'slate-divider-face))
      (widget-insert (propertize (number-to-string line-number) 'face 'slate-line-number-face))
      (widget-insert (make-string (- slate-max-file-name-length file-name-length) ? ))
      (widget-insert " ")
      (widget-insert (propertize todo 'face 'slate-todo-face))
      (widget-insert "\n"))))

(defun slate-buffer-setup (&optional refresh)
  (let ((orig-line (line-number-at-pos))
        (orig-col (current-column)))
    (let ((inhibit-read-only t))
      (erase-buffer))
    (remove-overlays)
    (slate-print-header)

    (if (eq 0 (length slate-todos))
        (widget-insert "No TODO entries found.")
      (mapc 'slate-todo-widget slate-todos))

    (widget-setup)

    (goto-char (point-min))
    (forward-line (if refresh (1- orig-line) 2))
    (forward-char (if refresh orig-col 0))))

;; Mode definition

(put 'slate-mode 'mode-class 'special)

(defun slate-mode ()
  "Major mode for quickly viewing all org TODOs in a directory."
  (kill-all-local-variables)
  (setq truncate-lines t)
  (setq buffer-read-only t)
  (setq default-directory (expand-file-name slate-directory))
  (when (fboundp 'visual-line-mode)
    (visual-line-mode 0))
  (setq major-mode 'slate-mode)
  (setq mode-name "Slate")
  (slate-find-todos)
  (slate-find-max-file-name-length)
  (slate-buffer-setup)
  )

(put 'slate-mode 'mode-class 'special)

;;;###autoload
(defun slate ()
  "Switch to *Slate* buffer and gather TODOs."
  (interactive)
  (switch-to-buffer slate-buffer)
  (if (not (eq major-mode 'slate-mode))
      (slate-mode)))

(provide 'slate)
