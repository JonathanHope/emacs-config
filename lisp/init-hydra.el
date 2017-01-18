;; Package configuration for hydra.

(use-package hydra
  :ensure t

  :config
  ;; Apps Hydra
  (defhydra apps-hydra (:color blue :columns 4)
    "Apps"
    ("o" org-mode-launch "Org-mode")
    ("m" magit-mode-launch "Magit")
    ("d" dired "Dired")
    ("s" eshell "Shell")
    ("c" launch-calc "Calculator")
    ("r" rainbow-mode "Rainbow mode")
    ("l" flyspell-mode "Spell check")
    ("k" describe-key "Describe key")
    ("f" describe-function "Describe function")
    ("e" describe-mode "Describe mode")
    ("q" nil "Exit"))

  ;; Org-mode Hydras
  	
  ;; Top org-mode hydra, serves as a launcher for other hydras.
  (defhydra org-hydra-top (:color blue :columns 4)
    "Org-mode"
    ("t" (progn
           (org-hydra-todo/body)
           (hydra-push '(org-hydra-top/body))) "Todo")
    ("l" (progn
           (org-hydra-link/body)
           (hydra-push '(org-hydra-top/body))) "Link")
    ("v" (progn
           (org-hydra-visibility/body)
           (hydra-push '(org-hydra-top/body))) "Visibility")
    ("c" (progn
           (org-hydra-checkbox/body)
           (hydra-push '(org-hydra-top/body))) "Checkbox")
    ("g" (progn
           (org-hydra-tags/body)
           (hydra-push '(org-hydra-top/body))) "Tag")
    ("b" (progn
           (org-hydra-tables/body)
           (hydra-push '(org-hydra-top/body))) "Table")
    ("s" (progn
           (org-hydra-subtree/body)
           (hydra-push '(org-hydra-top/body))) "Subtree")
    ("f" (progn
           (org-hydra-footnotes/body)
           (hydra-push '(org-hydra-top/body))) "Footnote")
    ("p" (progn
           (org-hydra-properties/body)
           (hydra-push '(org-hydra-top/body))) "Properties")
    ("o" (progn
         (org-hydra-time/body)
         (hydra-push '(org-hydra-top/body))) "Time")
    ("e" org-attach "Attach file")
    ("x" org-export-dispatch "Export file")
    ("a" org-agenda "Agenda")
    ("q" hydra-pop "Exit"))

  ;; Hydra for org-mode todo related items.
  (defhydra org-hydra-todo (:color blue :columns 4)
    "Org-mode todo"
    ("n" org-insert-todo-heading "New todo")
    ("t" org-todo "Toggle todo")
    ("s" org-schedule "Schedule todo")
    ("s" org-deadline "Deadline todo")
    ("p" org-priority "Prioritize todo")
    ("u" org-do-promote "Promote todo")
    ("d" org-do-demote "Demote todo")
    ("q" hydra-pop "Exit"))

  ;; Hydra for org-mode link related items.
  (defhydra org-hydra-link (:color blue :columns 4)
    "Org-mode link"
    ("n" org-insert-link "New link")
    ("o" org-open-at-point "Open link")
    ("q" hydra-pop "Exit"))

  ;; Hydra for org-mode visibility related items.
  (defhydra org-hydra-visibility (:color blue :columns 4)
    "Org-mode visibility"
    ("a" outline-show-all "Show all")
    ("o" outline-hide-body "Show outline")
    ("s" outline-hide-sublevels "Show sections")
    ("n" org-narrow-to-subtree "Narrow to subtree")
    ("w" widen "Widen")
    ("t" org-sparse-tree "Narrow to sparse tree")
    ("q" hydra-pop "Exit"))

  ;; Hydra for org-mode checkbox related items.
  (defhydra org-hydra-checkbox (:color blue :columns 4)
    "Org-mode checkbox"
    ("n" org-insert-todo-heading "New checkbox")
    ("t" org-toggle-checkbox "Toggle checkbox")
    ("q" hydra-pop "Exit"))

  ;; Hydra for org-mode tag related items.
  (defhydra org-hydra-tags (:color blue :columns 4)
    "Org-mode tags"
    ("s" org-set-tags-command "Set tags")
    ("q" hydra-pop "Exit"))

  ;; Hydra for org-mode table related items.
  (defhydra org-hydra-tables (:color blue :columns 4)
    "Org-mode tables"
    ("n" org-table-create "New table")
    ("a" org-table-align "Align table")
    ("r" org-table-insert-row "Insert row")
    ("c" org-table-insert-column "Insert column")
    ("d" org-table-delete-column "Delete column")
    ("q" hydra-pop "Exit"))

  ;; Hydra for org-mode subtree related items.
  (defhydra org-hydra-subtree (:color blue :columns 4)
    "Org-mode subtree"
    ("x" org-cut-subtree "Cut subtree")
    ("c" org-copy-subtree "Copy subtree")
    ("u" org-promote-subtree "Promote subtree")
    ("d" org-demote-subtree "Demote subtree")
    ("q" hydra-pop "Exit"))

  ;; Hydra for org-mode footnote related items.
  (defhydra org-hydra-footnotes (:color blue :columns 4)
    "Org-mode footnotes"
    ("n" org-footnote-action "New footnote")
    ("r" org-footnote-reference "New footnote reference")
    ("q" hydra-pop "Exit"))

  ;; Hydra for org-mode property related items.
  (defhydra org-hydra-properties (:color blue :columns 4)
    "Org-mode properties"
    ("n" org-set-property "New property")
    ("d" org-delete-property "Delete property")
    ("q" hydra-pop "Exit"))

  ;; Hydra for org-mode time related items.
  (defhydra org-hydra-time (:color blue :columns 4)
    "Org-mode time"
    ("i" org-clock-in "Clock in")
    ("o" org-clock-out "Clock out")
    ("r" org-evaluate-time-range "Refresh range")
    ("t" org-clock-report "Generate Report")
    ("c" org-clock-cancel "Cancel")
    ("q" hydra-pop "Exit"))

  ;; Clojure Hydra
  (defhydra clojure-hydra (:color blue :columns 4)
    "Clojure"
    ("r" cider-jack-in "Launch REPL")
    ("n" cider-repl-set-ns "Set REPL namespace")
    ("l" cider-load-buffer "Load buffer in REPL")
    ("s" cider-eval-last-sexp "Evaluate sexp")
    ("k" cider-quit "Close the REPL")
    ("q" nil "Exit"))

  ;; Markdown Hydra
  (defhydra markdown-hydra (:color blue :columns 4)
    "Markdown"
    ("l" livedown-preview "Live Preview")
    ("k" livedown-kill "End Live Preview")
    ("q" nil "Exit"))

  ;; SQL Hydra
  (defhydra sql-hydra (:color blue :columns 4)
    "Microsoft SQL Server"
    ("s" start-sql-ss "Connect to Database")
    ("b" sql-send-buffer-ss "Load buffer in REPL")
    ("q" nil "Exit"))

  ;; Javascript Hydra
  (defhydra javascript-hydra (:color blue :columns 4)
    "Javascript"
    ("r" launch-js-repl "Launch REPL")
    ("b" nodejs-repl-send-buffer "Load buffer")
    ("s" nodejs-repl-send-region "Send selection")
    ("i" install-npm-package "Install NPM package")
    ("q" nil "Exit"))

  ;; Emacs Lisp Hydra
  (defhydra elisp-hydra (:color blue :columns 4)
    "Emacs Lisp"
    ("r" launch-elisp-repl "Launch REPL")
    ("q" nil "Exit"))

  ;; Dired Hydra
  (defhydra dired-hydra (:color blue :columns 4)
    "Dired"
    ("m" dired-mark "Mark file")
    ("u" unmark-file "Unmark file")
    ("U" dired-unmark-all-marks "Unmark all")
    ("<enter>" dired-find-file "Open")
    ("C" dired-do-copy "Copy")
    ("R" dired-do-rename "Rename")
    ("D" dired-do-delete "Delete")
    ("+" dired "Create directory")
    ("q" nil "Exit"))

    ;; HTML Hydra
  (defhydra html-hydra (:color blue :columns 4)
    "Dired"
    ("r" launch-browser-repl "Launch REPL")
    ("q" nil "Exit"))
  
  ;; Support for nested hydras.
  (defvar hydra-stack nil)

  (defun hydra-push (expr)
    "Push a hydra onto the stack."
    (push `(lambda () ,expr) hydra-stack))

  (defun hydra-pop ()
    "Pop a hydra off the stack."
    (interactive)
    (let ((x (pop hydra-stack)))
      (when x
        (funcall x))))

  (defun org-footnote-reference (arg)
  "Insert a reference to a org-mode footnote."
    (interactive
      (list
      (read-string "Footnote: ")))
    (insert "[fn:" arg "]"))

  (defun org-mode-launch ()
    "Launch org-mode in the correct directory."
    (interactive)
    (setq default-directory notes-directory)
    (org-mode))

  (defun magit-mode-launch ()
    "Launch org-mode in the correct directory."
    (interactive)
    (magit-status)
    (delete-other-windows))

  (defun sql-add-newline-fi rst (output)
    "Add two new lines to ths tart of comint output."
    (remove-hook 'comint-preoutput-filter-functions
             'sql-add-newline-first)
    (concat "\n\n" output))

  (defun sql-send-buffer-ss ()
    "Sends a buffer to Microsft SQL Server interpreter. Removes line breaks and adds the end of query identifier."
    (interactive)
    (add-hook 'comint-preoutput-filter-functions 'sql-add-newline-first)
    (let ((sql-str (buffer-substring-no-properties (point-min) (point-max))))
      (let ((sql-str-flattened (concat (replace-regexp-in-string "\n" " " sql-str t t) " SENDQUERY")))
        (sql-send-string sql-str-flattened))))

  (defun start-sql-ss ()
    "Connect to a Microsoft SQL Server database."
    (interactive)
    (sql-ss)
    (select-window-1))

  (defun launch-elisp-repl ()
    "Launch an elisp REPL."
    (interactive)
    (split-window-horizontally)
    (select-window-2)
    (ielm))

  (defun launch-js-repl ()
    "Launch a javascript REPL."
    (interactive)
    (nodejs-repl)
    (select-window-1))

  (defun install-npm-package (arg)
    "Install a npm package."
    (interactive
      (list
       (read-string "Package: ")))
    (shell-command-to-string (concat "npm install " arg)))

  (defun launch-calc ()
    (interactive)
    (calc)
    (calc-trail-display 0))

  (defun launch-browser-repl ()
    (interactive)
    (httpd-start)
    (impatient-mode)
    (shell-command-to-string "start http://localhost:8080/imp/")))

(provide 'init-hydra)
