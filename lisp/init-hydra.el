;; Package configuration for hydra.

(use-package hydra
  :ensure t
  :defer t

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
    ("h" (progn
           (apps-hydra-describe/body)
           (hydra-push '(apps-hydra/body))) "Describe")
    ("q" nil "Exit"))

  (defhydra apps-hydra-describe (:color blue :columns 4)
    "Org-mode todo"
    ("m" describe-mode "Describe mode")
    ("f" counsel-describe-function "Describe function")
    ("k" counsel-descbinds "Describe key")
    ("v" counsel-describe-variable "Describe variable")
    ("a" counsel-faces "Describe face")
    ("q" hydra-pop "Exit"))

  ;; Org-mode Hydras

  ;; Top org-mode hydra, serves as a launcher for other hydras.
  (defhydra org-hydra-top (:color blue :columns 4)
    "Org-mode"
    ("a" (progn
           (org-hydra-agenda/body)
           (hydra-push '(org-hydra-top/body))) "Agenda")
    ("b" (progn
           (org-hydra-tables/body)
           (hydra-push '(org-hydra-top/body))) "Table")
    ("c" (progn
           (org-hydra-checkbox/body)
           (hydra-push '(org-hydra-top/body))) "Checkbox")
    ("d" (progn
           (org-hydra-drawer/body)
           (hydra-push '(org-hydra-top/body))) "Drawer")
    ("e" org-attach "Attach file")
    ("f" (progn
           (org-hydra-footnotes/body)
           (hydra-push '(org-hydra-top/body))) "Footnote")
    ("g" (progn
           (org-hydra-tags/body)
           (hydra-push '(org-hydra-top/body))) "Tag")
    ("i" (progn
           (org-hydra-priority/body)
           (hydra-push '(org-hydra-top/body))) "Priority")
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
    ("x" (progn
           (org-hydra-export/body)
           (hydra-push '(org-hydra-top/body))) "Export")
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

  ;; Hydra for org-mode agenda related items.
  (defhydra org-hydra-agenda (:color blue :columns 4)
    "Org-mode agenda"
    ("t" org-todo-list "TODO List")
    ("g" org-tags-list "Tags List")
    ("h" counsel-org-agenda-headlines "Headline List")
    ("q" hydra-pop "Exit"))

  ;; Hydra for org-mode link related items.
  (defhydra org-hydra-link (:color blue :columns 4)
    "Org-mode link"
    ("n" org-insert-link "New link")
    ("f" org-insert-file-link "New file link")
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

  ;; Hydra for org-mode source related items.
  (defhydra org-hydra-source (:color blue :columns 4)
    "Org-mode source"
    ("e" org-edit-src-code "Edit source")
    ("n" org-insert-src-block "New source")
    ("x" org-babel-execute-src-block "Execute source")
    ("c" cider-jack-in "Start Clojure backend")
    ("q" hydra-pop "Exit"))

  ;; Hydra for org-mode export related items.
  (defhydra org-hydra-export (:color blue :columns 4)
    "Org-mode source"
    ("c" org-confluence-export-as-confluence "Export as confluence")
    ("h" org-html-export-as-html "Export as html")
    ("l" org-latex-export-as-latex "Export as LaTeX")
    ("q" hydra-pop "Exit"))

  ;; Hydra for org-mode priority related items.
  (defhydra org-hydra-priority (:color blue :columns 4)
    "Org-mode priority"
    ("s" org-priority "Set priority")
    ("u" org-priority-up "Priority up")
    ("d" org-priority-down "Priority down")
    ("q" hydra-pop "Exit"))

  ;; Hydra for org-mode drawer related items.
  (defhydra org-hydra-drawer (:color blue :columns 4)
    "Org-mode drawers"
    ("n" org-insert-drawer "Org insert drawer")
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

  (defun org-insert-src-block ()
    (interactive)
    (ivy-read "Source  block language: "
              '("clojure" "csharp" "sql" "restclient")
              :require-match t
              :sort t
              :action (lambda (src-code-type)
                        (progn
                          (insert (format "#+BEGIN_SRC %s :results output\n" src-code-type))
                          (newline-and-indent)
                          (insert "#+END_SRC\n")
                          (previous-line 2)
                          (org-edit-src-code)))))

  (defun org-insert-file-link ()
    (interactive)
    (ivy-read "File: "
              'read-file-name-internal
              :matcher #'counsel--find-file-matcher
              :require-match t
              :sort t
              :action (lambda (file-name)
                        (progn
                          (insert (format "[[file:%s]]" file-name))
                          (org-redisplay-inline-images))))))

(provide 'init-hydra)
