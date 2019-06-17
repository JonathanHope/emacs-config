;;; slate.el --- Master TODO list.

;; Copyright (C) 2019 Jonathan Hope

;; Author: Jonathan Hope <jonathan.douglas.hope@gmail.com>
;; Version: 1.0
;; Package-Requires ()
;; Keywords: todo

;; TODO: Add ability to exclude archive directory.
;; TODO: Sort priority buckets by name.
;; TODO: Limit TODO text to screen width.
;; TODO: Show tags.
;; TODO: Add incremental regex filtering.
;; TODO: Add priority filtering.
;; TODO: Add tag filtering.

;;; Commentary:

;; slate is a major mode that gathers the TODOS from all org files in a directory.
;; Those TODOS are then displayed in a list that can be used to visit those files.

;;; Code:

(require 'subr-x)
(require 'cl)

;; Customization

(defgroup slate nil
  "Emacs Slate mode."
  :group 'local)

(defcustom slate-directory (expand-file-name "~/Notes/")
  "Directory containing org files to find TODOs in."
  :type 'directory
  :safe 'stringp
  :group 'slate)

(defcustom slate-rg "rg"
  "Location of ripgrep executable."
  :type 'string
  :group 'slate)

(defcustom slate-todo-limit 0
  "Number of TODOs to render. No limit if zero."
  :type 'number
  :group 'slate)

(defcustom slate-file-name-limit 40
  "Max length of file name."
  :type 'number
  :group 'slate)

(defcustom slate-ellipsis "…"
  "What to denote a truncated string with."
  :type 'string
  :group 'slate)

(defcustom slate-ellipsis-width 2
  "The actual width of the ellipsis. It could be a double width character."
  :type 'number
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

(defface slate-file-name-face
  '((t :inherit font-lock-function-name-face :bold t))
  "Face for Slate file name."
  :group 'slate-faces)

(defface slate-divider-face
  '((t :inherit font-lock-builtin-face :bold t))
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

(defface slate-priority-a-face
  '((t :inherit font-lock-warning-face :bold t))
  "Face for Slate priority a indicator."
  :group 'slate-faces)

(defface slate-priority-b-face
  '((t :inherit font-lock-variable-name-face :bold t))
  "Face for Slate priority b indicator."
  :group 'slate-faces)

(defface slate-priority-c-face
  '((t :inherit font-lock-type-face :bold t))
  "Face for Slate priority c indicator."
  :group 'slate-faces)

;; Constants

(defconst slate-buffer "*Slate*"
  "Slate buffer name.")

;; Variables

(defvar slate-todos nil
  "The unfiltered TODOs gathered from the org files.")

(defvar slate-filtered-todos nil
  "The filtered TODOs that are displayed in the slate buffer.")

(defvar slate-max-file-name-length nil
  "The length of the longest file name in the slate-todos list.")

;; Keymap definition

(defvar slate-mode-map
  (let ((i 0)
        (map (make-keymap)))
    (define-key map (kbd "RET") 'slate-open)
    map)
  "Keymap for Slate mode.")

;; Low level helpers

(defun get-current-line-number ()
  "Get the line number the cursor is on currently."
  (interactive)
  (save-restriction
    (widen)
    (save-excursion
      (beginning-of-line)
      (1+ (count-lines 1 (point))))))

(defun slate-append-list-to-list (lists list)
  "Append a list to a list of lists."
  (append lists (list list)))

(defun slate-append-value-to-list (list value)
  "Appends a value to a list."
  (append list (list value)))

;; Building the model

(defun slate-ripgrep-todos ()
  "Use ripgrep to find all of the TODO items in org files in a direct."
  (if default-directory
      (shell-command-to-string (concat slate-rg " --line-number --no-messages --color never --no-heading \"^\\*. ?TODO \" --iglob \"*.org\" "  default-directory))
    nil))

(defun slate-string-to-lines (ripgrep-output)
  "Split a string into lines."
  (if ripgrep-output
      (split-string ripgrep-output "\n")
    nil))

(defun slate-filter-empty-strings (todos)
  "Filter empty lines out of a list of lines."
  (if todos
      (seq-filter (lambda (elt)
                    (not (string= elt "")))
                  todos)
    nil))

(defun slate-remove-drive-letters (todos)
  "Remove any Windows driver letters from a list of strings."
  (if todos
      (mapcar (lambda (elt)
                (replace-regexp-in-string "^[a-zA-Z]:/" "" elt))
              todos)
    nil))

(defun slate-get-file-paths (todos)
  "Get the full file paths from the todos."
  (if todos
      (mapcar (lambda (elt)
                (replace-regexp-in-string ":[0-9]+:.*" "" elt))
              todo-lines-non-empty)
    nil))

(defun slate-get-priorities (todos)
  "Get the priorities from the todos."
  (if todos
      (let ((regex "\\(\\[#[A-C]\\]\\)"))
        (mapcar (lambda (elt)
                  (if (string-match-p regex elt)
                      (progn
                        (string-match "\\(\\[#[A-C]\\]\\)" elt)
                        (substring (match-string 0 elt) 2 -1))
                    " "))
                todos))
    nil))

(defun slate-tokenize (todos)
  "Tokenize a list of strings by :."
  (if todos
      (mapcar (lambda (elt)
                (split-string elt ":"))
              todos)
    nil))

(defun slate-filter-todo-text (todo-text)
  "Filter to just the todo text."
  (if todo-text
      (string-trim
       (replace-regexp-in-string "^\\*. ?TODO \\(\\[#[A-C]\\]\\)?" "" todo-text))
    nil))

(defun slate-calc-file-name-length (file-name line-number)
  "Get the length of the filename for a todo item."
  (if (and file-name line-number)
      (let* ((file-name-length (if (> (length file-name) slate-file-name-limit)
                                   (- slate-file-name-limit 1)
                                 (length file-name)))
             (file-name-length-with-line-number (+ file-name-length
                                                   (length (number-to-string line-number))
                                                   1)))
        file-name-length-with-line-number)
    0))

(defun slate-build-todos (todos file-paths priorities)
  "Build the final todos list object."
  (if (and todos file-paths priorities)
      (let ((index 0)
            (processed-todos '()))
        (while (< index (length todos))
          (let* ((todo (nth index todos))
                 (file-name (file-name-nondirectory (nth 0 todo)))
                 (line-number (string-to-number (nth 1 todo)))
                 (priority (nth index priorities))
                 (todo-text (slate-filter-todo-text (nth 2 todo)))
                 (file-path (nth index file-paths))
                 (file-name-length (slate-calc-file-name-length file-name line-number)))
            (setq index (1+ index))
            (setq processed-todos (slate-append-list-to-list processed-todos
                                                             (let ((todo-hash-table (make-hash-table :test 'equal)))
                                                               (puthash "file-name" file-name todo-hash-table)
                                                               (puthash "line-number" line-number todo-hash-table)
                                                               (puthash "priority" priority todo-hash-table)
                                                               (puthash "todo-text" todo-text todo-hash-table)
                                                               (puthash "file-path" file-path todo-hash-table)
                                                               (puthash "file-name-length" file-name-length todo-hash-table)
                                                               todo-hash-table)))))
        processed-todos)
    nil))

(defun slate-sort-todos (todos)
  "Sort the todos into priority buckets."
  (if todos
      (let ((priority-a-todos '())
            (priority-b-todos '())
            (priority-c-todos '())
            (priority-none-todos '()))
        (progn
          (dolist (todo todos)
            (let ((priority (gethash "priority" todo)))
              (cond ((equal "A" priority)
                     (setq priority-a-todos (slate-append-value-to-list priority-a-todos todo)))
                    ((equal "B" priority)
                     (setq priority-b-todos (slate-append-value-to-list priority-b-todos todo)))
                    ((equal "C" priority)
                     (setq priority-c-todos (slate-append-value-to-list priority-c-todos todo)))
                    ((equal " " priority)
                     (setq priority-none-todos (slate-append-value-to-list priority-none-todos todo))))))
          (append priority-none-todos
                  priority-a-todos
                  priority-b-todos
                  priority-c-todos)))
    nil))

(defun slate-find-todos ()
  "Find all of the TODOs in org files in a given directory."
  (let* ((rg-output (slate-ripgrep-todos))
         (todo-lines (slate-string-to-lines rg-output))
         (todo-lines-non-empty (slate-filter-empty-strings todo-lines))
         (todo-lines-non-empty-no-drive-letter (slate-remove-drive-letters todo-lines-non-empty))
         (full-file-paths (slate-get-file-paths todo-lines-non-empty))
         (priorities (slate-get-priorities todo-lines-non-empty))
         (todos-tokens (slate-tokenize todo-lines-non-empty-no-drive-letter))
         (todos (slate-build-todos todos-tokens full-file-paths priorities))
         (sorted-todos (slate-sort-todos todos)))
    (setq slate-todos sorted-todos)))

;; Filtering the model.

(defun slate-limit-todos (todos)
  "Limit the number of TODOs that are being rendered."
  (if todos
      (if (< 0 slate-todo-limit)
          (seq-take todos slate-todo-limit)
        todos)
    nil))

(defun slate-filter-todos ()
  "Apply any filters to the current TODOs."
  (let* ((limited-todos (slate-limit-todos slate-todos)))
    (setq slate-filtered-todos limited-todos)))

;; Geometry calculations.

(defun slate-find-max-file-name-length (max-file-name-lengths)
  "Find the length of the longest file name amongst the file names of the TODOs."
  (if max-file-name-lengths
      (let ((file-name-lengths (mapcar (lambda (elt)
                                         (gethash "file-name-length" elt))
                                       max-file-name-lengths)))
        (reduce #'max file-name-lengths))
    0))

(defun slate-calculate-geometry ()
  "Do any calcualtions needed to draw the UI later."
  (let* ((max-file-name-length (slate-find-max-file-name-length slate-filtered-todos)))
    (setq slate-max-file-name-length max-file-name-length)))

;; Binding the model to the UI

(defun slate-truncate-string (value max-length)
  "Truncate a string to a length and append an ellipsis to it."
  (if (> (length value) max-length)
      (concat (substring value 0 (- max-length slate-ellipsis-width)) slate-ellipsis)
    value))

(defun slate-print-header ()
  "Print the header to slate buffer."
  (let ((inhibit-read-only t))
    (insert (propertize "Slate" 'face 'slate-header-face))
    (insert "\n\n")))

(defun slate-print-todo (todo)
  "Print the TODOs to the slate buffer."
  (let ((file-name (slate-truncate-string (gethash "file-name" todo) slate-file-name-limit))
        (line-number (gethash "line-number" todo))
        (priority (gethash "priority" todo))
        (todo-text (gethash "todo-text" todo))
        (file-name-length (gethash "file-name-length" todo))
        (inhibit-read-only t))
    (cond ((equal "A" priority) (insert (propertize priority 'face 'slate-priority-a-face)))
          ((equal "B" priority) (insert (propertize priority 'face 'slate-priority-b-face)))
          ((equal "C" priority) (insert (propertize priority 'face 'slate-priority-c-face)))
          ((equal " " priority) (insert " ")))
    (insert " ")
    (insert (propertize file-name 'face 'slate-file-name-face))
    (insert (propertize ":" 'face 'slate-divider-face))
    (insert (propertize (number-to-string line-number) 'face 'slate-line-number-face))
    (if (< file-name-length slate-max-file-name-length)
        (insert (make-string (- slate-max-file-name-length file-name-length) ? )))
    (insert " ")
    (insert (propertize todo-text 'face 'slate-todo-face))
    (insert "\n")))

;; Externally useful functions

(defun slate-refresh ()
  "Refresh the TODO list."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer))
  (remove-overlays)
  (slate-print-header)
  (if (executable-find slate-rg)
      (progn
        (slate-find-todos)
        (slate-filter-todos)
        (slate-calculate-geometry)
        (if (eq 0 (length slate-todos))
            (let ((inhibit-read-only t))
              (insert "Nothing slated."))
          (mapc 'slate-print-todo slate-filtered-todos))
        (goto-char (point-min))
        (forward-line 2)
        (forward-char 0))
    (let ((inhibit-read-only t))
      (insert "Ripgrep not found."))))

;; Events

(defun slate-open ()
  "Open the file under the cursor and go to the line number of the TODO."
  (interactive)
  (let* ((current-line-number (get-current-line-number))
         (todo-index (- current-line-number 3)))
    (if (and (>= todo-index 0)
             (< todo-index (length slate-todos)))
        (let* ((todo (nth todo-index slate-todos))
               (file-path (gethash "file-path" todo))
               (line-number (gethash "line-number" todo)))
          (find-file file-path)
          (goto-line line-number)))))

;; Mode definition

(put 'slate-mode 'mode-class 'special)

(defun slate-mode ()
  "Major mode for quickly viewing all org TODOs in a directory."
  (kill-all-local-variables)
  (setq truncate-lines t)
  (setq buffer-read-only t)
  (if (file-directory-p slate-directory)
      (setq default-directory (expand-file-name slate-directory)))
  (when (fboundp 'visual-line-mode)
    (visual-line-mode 0))
  (use-local-map slate-mode-map)
  (setq major-mode 'slate-mode)
  (setq mode-name "Slate")
  (slate-refresh))

(put 'slate-mode 'mode-class 'special)

;;;###autoload
(defun slate ()
  "Switch to *Slate* buffer and gather TODOs."
  (interactive)
  (switch-to-buffer slate-buffer)
  (if (not (eq major-mode 'slate-mode))
      (slate-mode)))

(provide 'slate)

;; slate.el ends here