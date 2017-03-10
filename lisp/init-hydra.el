;; Package configuration for hydra.

(use-package hydra
  :ensure t

  :config
  ;; Apps Hydra
  (defhydra apps-hydra (:color blue :columns 4)
    "Apps"
    ("c" launch-calc "Calculator")
    ("d" dired "Dired")
    ("m" magit-mode-launch "Magit")
    ("o" org-mode-launch "Org-mode")
    ("s" eshell "Shell")
    ("l" flyspell-mode "Spell check")
    ("r" rainbow-mode "Rainbow mode")
    ("t" sort-lines "Sort lines")
    ("e" describe-mode "Describe mode")
    ("f" counsel-describe-function "Describe function")
    ("k" describe-key "Describe key")
    ("v" counsel-describe-variable "Describe variable")
    ("q" nil "Exit"))

  ;; Org-mode Hydras

  ;; Top org-mode hydra, serves as a launcher for other hydras.
  (defhydra org-hydra-top (:color blue :columns 4)
    "Org-mode"
    ("a" org-agenda "Agenda")
    ("b" (progn
           (org-hydra-tables/body)
           (hydra-push '(org-hydra-top/body))) "Table")
    ("c" (progn
           (org-hydra-checkbox/body)
           (hydra-push '(org-hydra-top/body))) "Checkbox")
    ("e" org-attach "Attach file")
    ("f" (progn
           (org-hydra-footnotes/body)
           (hydra-push '(org-hydra-top/body))) "Footnote")
    ("g" (progn
           (org-hydra-tags/body)
           (hydra-push '(org-hydra-top/body))) "Tag")
    ("l" (progn
           (org-hydra-link/body)
           (hydra-push '(org-hydra-top/body))) "Link")
    ("o" (progn
           (org-hydra-time/body)
           (hydra-push '(org-hydra-top/body))) "Time")
    ("p" (progn
           (org-hydra-properties/body)
           (hydra-push '(org-hydra-top/body))) "Properties")
    ("r" (progn
           (org-hydra-source/body)
           (hydra-push '(org-hydra-top/body))) "Source")
    ("s" (progn
           (org-hydra-subtree/body)
           (hydra-push '(org-hydra-top/body))) "Subtree")
    ("t" (progn
           (org-hydra-todo/body)
           (hydra-push '(org-hydra-top/body))) "Todo")
    ("v" (progn
           (org-hydra-visibility/body)
           (hydra-push '(org-hydra-top/body))) "Visibility")
    ("x" org-export-dispatch "Export file")
    ("q" hydra-pop "Exit"))

  ;; Hydra for org-mode todo related items.
  (defhydra org-hydra-todo (:color blue :columns 4)
    "Org-mode todo"
    ("d" org-do-demote "Demote todo")
    ("l" org-deadline "Deadline todo")
    ("p" org-priority "Prioritize todo")
    ("n" org-insert-todo-heading "New todo")
    ("s" org-schedule "Schedule todo")
    ("t" org-todo "Toggle todo")
    ("u" org-do-promote "Promote todo")
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
    ("n" org-narrow-to-subtree "Narrow to subtree")
    ("o" outline-hide-body "Show outline")
    ("s" outline-hide-sublevels "Show sections")
    ("t" org-sparse-tree "Narrow to sparse tree")
    ("w" widen "Widen")
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
    ("a" org-table-align "Align table")
    ("c" org-table-insert-column "Insert column")
    ("d" org-table-delete-column "Delete column")
    ("n" org-table-create "New table")
    ("r" org-table-insert-row "Insert row")
    ("q" hydra-pop "Exit"))

  ;; Hydra for org-mode subtree related items.
  (defhydra org-hydra-subtree (:color blue :columns 4)
    "Org-mode subtree"
    ("c" org-copy-subtree "Copy subtree")
    ("d" org-demote-subtree "Demote subtree")
    ("u" org-promote-subtree "Promote subtree")
    ("x" org-cut-subtree "Cut subtree")
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
    ("d" org-delete-property "Delete property")
    ("n" org-set-property "New property")
    ("q" hydra-pop "Exit"))

  ;; Hydra for org-mode time related items.
  (defhydra org-hydra-time (:color blue :columns 4)
    "Org-mode time"
    ("c" org-clock-cancel "Cancel")
    ("i" org-clock-in "Clock in")
    ("o" org-clock-out "Clock out")
    ("r" org-evaluate-time-range "Refresh range")
    ("t" org-clock-report "Generate Report")
    ("q" hydra-pop "Exit"))

  ;; Hydra for source related items.
  (defhydra org-hydra-source (:color blue :columns 4)
    "Org-mode source"
    ("e" org-edit-src-code "Edit source")
    ("n" org-insert-src-block "New source")
    ("q" hydra-pop "Exit"))

  ;; Clojure Hydra
  (defhydra clojure-hydra (:color blue :columns 4)
    "Clojure"
    ("k" cider-quit "Close the REPL")
    ("l" cider-load-buffer "Load buffer in REPL")
    ("n" cider-repl-set-ns "Set REPL namespace")
    ("r" cider-jack-in "Launch REPL")
    ("s" cider-eval-last-sexp "Evaluate sexp")
    ("q" nil "Exit"))

  ;; Markdown Hydra
  (defhydra markdown-hydra (:color blue :columns 4)
    "Markdown"
    ("k" livedown-kill "End Live Preview")
    ("l" livedown-preview "Live Preview")
    ("q" nil "Exit"))

  ;; SQL Hydra
  (defhydra sql-hydra (:color blue :columns 4)
    "Microsoft SQL Server"
    ("b" sql-send-buffer-ss "Load buffer in REPL")
    ("s" start-sql-ss "Connect to Database")
    ("q" nil "Exit"))

  ;; Javascript Hydra
  (defhydra javascript-hydra (:color blue :columns 4)
    "Javascript"
    ("b" nodejs-repl-send-buffer "Load buffer")
    ("i" install-npm-package "Install NPM package")
    ("r" launch-js-repl "Launch REPL")
    ("s" nodejs-repl-send-region "Send selection")
    ("q" nil "Exit"))

  ;; Emacs Lisp Hydra
  (defhydra elisp-hydra (:color blue :columns 4)
    "Emacs Lisp"
    ("r" launch-elisp-repl "Launch REPL")
    ("q" nil "Exit"))

  ;; Dired Hydra
  (defhydra dired-hydra (:color blue :columns 4)
    "Dired"
    ("+" dired "Create directory")
    ("<enter>" dired-find-file "Open")
    ("C" dired-do-copy "Copy")
    ("D" dired-do-delete "Delete")
    ("m" dired-mark "Mark file")
    ("R" dired-do-rename "Rename")
    ("u" unmark-file "Unmark file")
    ("U" dired-unmark-all-marks "Unmark all")
    ("q" nil "Exit"))

  (defhydra html-hydra (:color blue :columns 4)
    "Dired"
    ("r" launch-browser-repl "Launch REPL")
    ("q" nil "Exit"))

  (defhydra bat-hydra (:color blue :columns 4)
    "Bat"
    ("r" bat-run "Run")
    ("a" bat-run-args "Run with args")
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
    (shell-command-to-string "start http://localhost:8080/imp/"))

  (setq org-insert-src-block-helm-source
        '((name . "Source block language")
          (candidates . ("emacs-lisp"  "C" "sh" "js" "clojure" "C++" "css" "csharp"))
          (action . (lambda (src-code-type)
                      (progn
                        (newline-and-indent)
                        (insert (format "#+BEGIN_SRC %s\n" src-code-type))
                        (newline-and-indent)
                        (insert "#+END_SRC\n")
                        (previous-line 2)
                        (org-edit-src-code))))))

  (defun org-insert-src-block ()
    (interactive
     (helm :sources '(org-insert-src-block-helm-source)))))

(provide 'init-hydra)
