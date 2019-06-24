;; Package configuration for hydra.

(use-package hydra
  :ensure t
  :defer t

  :config
  ;; Settings.

  (setq hydra-posframe-show-params
        '(
          :internal-border-width 0
          :internal-border-color "red"
          :poshandler posframe-poshandler-window-center))

  ;; Apps Hydra

  (defhydra mainspring-hydra-apps (
                                   :hint nil
                                   :pre (setq hydra-hint-display-type 'posframe)
                                   :post (setq hydra-hint-display-type 'lv))
    "
┏^^━━━━━━━━━━━┳^^━━━━━━━━━━━━━┳^^━━━━━━━━━━━━━┳^^━━━━━━━━━━━━┳^^━━━━━━━━━━━━━━━━━┓
┃^^ Apps      ┃^^ Minor Modes ┃^^ Describe    ┃^^ Languages  ┃^^ Zoom            ┃
┣^^━━━━━━━━━━━╋^^━━━━━━━━━━━━━╋^^━━━━━━━━━━━━━╋^^━━━━━━━━━━━━╋^^━━━━━━━━━━━━━━━━━┫
┃ _d_: Dired  ┃ _r_: Rainbow  ┃ _M_: Mode     ┃ _C_: Clojure ┃ _+_: Zoom In      ┃
┃ _m_: Magit  ┃ _l_: Flyspell ┃ _F_: Function ┃^^            ┃ _-_: Zoom Out     ┃
┃ _o_: Org    ┃^^             ┃ _K_: Key      ┃^^            ┃ _0_: Zoom Reset   ┃
┃ _s_: Eshell ┃^^             ┃ _V_: Variable ┃^^            ┃^^                 ┃
┃ _n_: Deft   ┃^^             ┃ _A_: Face     ┃^^            ┃^^                 ┃
┃ _t_: Slate  ┃^^             ┃^^             ┃^^            ┃^^                 ┃
┃ _c_: Calc   ┃^^             ┃^^             ┃^^            ┃^^                 ┃
┗^^━━━━━━━━━━━┻^^━━━━━━━━━━━━━┻^^━━━━━━━━━━━━━┻^^━━━━━━━━━━━━┻^^━━━━━━━━━━━━━━━━━┛
┏^^━━━━━━━━━━━━━━━━━━━━━━━━━┳^^━━━━━━━━━━━━━━━━━━━━┳^^━━━━━━━━━━━━━━━━━━━━━━━┓
┃^^ Resize Window           ┃^^ Select Window      ┃^^ Manage Window         ┃
┣^^━━━━━━━━━━━━━━━━━━━━━━━━━╋^^━━━━━━━━━━━━━━━━━━━━╋^^━━━━━━━━━━━━━━━━━━━━━━━┫
┃ Spliter Up: _<up>_        ┃ _1_: Select Window 1 ┃ _|_: Split Horizontally ┃
┃ Spliter Down: _<down>_    ┃ _2_: Select Window 2 ┃ ___: Split Vertically   ┃
┃ Splitter Left: _<left>_   ┃ _3_: Select Window 3 ┃ _x_: Delete             ┃
┃ Splitter Right: _<right>_ ┃ _4_: Select Window 4 ┃ _b_: Change Buffer      ┃
┗^^━━━━━━━━━━━━━━━━━━━━━━━━━┻^^━━━━━━━━━━━━━━━━━━━━┻^^━━━━━━━━━━━━━━━━━━━━━━━┛
"
    ("d" dired :color red)
    ("m" mainspring-hydra-magit-status :color red)
    ("o" mainspring-hydra-org-mode-launch :color red)
    ("s" eshell :color red)
    ("n" deft :color red)
    ("t" slate :color red)
    ("c" full-calc :color red)
    ("l" flyspell-mode :color red)
    ("r" rainbow-mode :color red)
    ("M" describe-mode :color blue)
    ("F" counsel-describe-function :color blue)
    ("K" counsel-descbinds :color blue)
    ("V" counsel-describe-variable :color blue)
    ("A" counsel-faces :color blue)
    ("C" mainspring-hydra-clojure-mode-launch :color red)
    ("+" text-scale-increase :color red)
    ("-" text-scale-decrease :color red)
    ("0" (text-scale-adjust 0) :color red)
    ("_" split-window-vertically :color red)
    ("|" split-window-horizontally :color red)
    ("x" delete-window :color red)
    ("1" winum-select-window-1 :color red)
    ("2" winum-select-window-2 :color red)
    ("3" winum-select-window-3 :color red)
    ("4" winum-select-window-4 :color red)
    ("<up>" mainspring-hydra-move-splitter-up :color red)
    ("<down>" mainspring-hydra-move-splitter-down :color red)
    ("<left>" mainspring-hydra-move-splitter-left :color red)
    ("<right>" mainspring-hydra-move-splitter-right :color red)
    ("b" ivy-switch-buffer :color red)
    ("q" nil :color blue))

  (defun mainspring-hydra-magit-status ()
    "Don't split window."
    (interactive)
    (let ((pop-up-windows nil))
      (call-interactively 'magit-status)))

  (defun mainspring-hydra-new-empty-buffer ()
    (interactive)
    (let (($buf (generate-new-buffer "untitled")))
      (switch-to-buffer $buf)
      $buf))

  (defun mainspring-hydra-org-mode-launch ()
    "Launch org-mode in the correct directory."
    (interactive)
    (mainspring-hydra-new-empty-buffer)
    (setq default-directory notes-directory)
    (org-mode))

  (defun mainspring-hydra-clojure-mode-launch ()
    "Launch clojure-mode."
    (interactive)
    (mainspring-hydra-new-empty-buffer)
    (clojure-mode))

  (defun mainspring-hydra-move-splitter-left (arg)
    "Move window splitter left."
    (interactive "p")
    (if (let ((windmove-wrap-around))
          (windmove-find-other-window 'right))
        (shrink-window-horizontally arg)
      (enlarge-window-horizontally arg)))

  (defun mainspring-hydra-move-splitter-right (arg)
    "Move window splitter right."
    (interactive "p")
    (if (let ((windmove-wrap-around))
          (windmove-find-other-window 'right))
        (enlarge-window-horizontally arg)
      (shrink-window-horizontally arg)))

  (defun mainspring-hydra-move-splitter-up (arg)
    "Move window splitter up."
    (interactive "p")
    (if (let ((windmove-wrap-around))
          (windmove-find-other-window 'up))
        (enlarge-window arg)
      (shrink-window arg)))

  (defun mainspring-hydra-move-splitter-down (arg)
    "Move window splitter down."
    (interactive "p")
    (if (let ((windmove-wrap-around))
          (windmove-find-other-window 'up))
        (shrink-window arg)
      (enlarge-window arg)))


  ;; Org-mode Hydras

  (defhydra mainspring-hydra-org (:hint nil)
    "
┏^^━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
┃^^ Org                         ┃
┣^^━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┫
┃ _h_: Headlines                ┃
┃ _t_: Tables                   ┃
┃ _l_: Links                    ┃
┃ _f_: Formatting               ┃
┃^^                             ┃
┃^^                             ┃
┃^^                             ┃
┃^^                             ┃
┃^^                             ┃
┗^^━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛
"
    ("h" (progn
           (mainspring-hydra-org-headline/body)
           (hydra-push '(mainspring-hydra-org/body))) :color blue)
    ("t" (progn
           (mainspring-hydra-org-table/body)
           (hydra-push '(mainspring-hydra-org/body))) :color blue)
    ("l" (progn
           (mainspring-hydra-org-link/body)
           (hydra-push '(mainspring-hydra-org/body))) :color blue)
    ("f" (progn
           (mainspring-hydra-org-formatting/body)
           (hydra-push '(mainspring-hydra-org/body))) :color blue)
    ("q" nil :color blue))

  (defhydra mainspring-hydra-org-headline (:hint nil)
    "
┏^^━━━━━━━━━━━━━━━━━━━━━━━━━━━^^━━━━━━━━━━━━━━━━━━┓
┃^^ Org - Headlines           ^^                  ┃
┣^^━━━━━━━━━━━━━━━━━━━━━━━━━━┳^^━━━━━━━━━━━━━━━━━━┫
┃ _n_: New Headline          ┃ _a_: Priority A    ┃
┃ _d_: Delete Headline       ┃ _b_: Priority B    ┃
┃ _s_: Set Headline Text     ┃ _c_: Priority C    ┃
┃ _<left>_: Promote Headline ┃ _r_: Priority NONE ┃
┃ _<right>_: Demote Headline ┃ _T_: Status TODO   ┃
┃ _M-<up>_: Headline Up      ┃ _D_: Status DONE   ┃
┃ _M-<down>_: Headline Down  ┃ _R_: Status NONE   ┃
┃ _<up>_: Previous Headline  ┃ _t_: Set Tags      ┃
┃ _<down>_: Next Headline    ┃^^                  ┃
┗^^━━━━━━━━━━━━━━━━━━━━━━━━━━┻^^━━━━━━━━━━━━━━━━━━┛
"
    ("n" mainspring-hydra-insert-headline :color red)
    ("d" mainspring-hydra-delete-headline :color red)
    ("s" org-edit-headline :color red)
    ("<left>" org-do-promote :color red)
    ("<right>" org-do-demote :color red)
    ("M-<up>" org-move-subtree-up :color red)
    ("M-<down>" org-move-subtree-down :color red)
    ("<down>" outline-next-heading :color red)
    ("<up>" outline-previous-heading :color red)
    ("a" mainspring-hydra-priority-a :color red)
    ("b" mainspring-hydra-priority-b :color red)
    ("c" mainspring-hydra-priority-c :color red)
    ("r" mainspring-hydra-priority-none :color red)
    ("T" mainspring-hydra-status-todo :color red)
    ("D" mainspring-hydra-status-done :color red)
    ("R" mainspring-hydra-status-none :color red)
    ("t" org-set-tags-command :color red)
    ("q" hydra-pop :color blue))

  (defhydra mainspring-hydra-org-table (:hint nil)
    "
┏^^━━━━━━━━━━━━━━━━━━━━━━━━━━━^^━━━━━━━━━━━━━━━━━━┓
┃^^ Org - Tables              ^^                  ┃
┣^^━━━━━━━━━━━━━━━━━━━━━━━━━━┳^^━━━━━━━━━━━━━━━━━━┫
┃ _n_: New Table             ┃ _c_: New Column    ┃
┃ _N_: New Table from Data   ┃ _r_: New Row       ┃
┃ _s_: Set Field Text        ┃ _d_: Delete Row    ┃
┃ _<backtab>_: Previous Cell ┃ _D_: Delete Column ┃
┃ _<tab>_: Next Cell         ┃ _a_: Align Table   ┃
┃ ^^                         ┃^^                  ┃
┃ ^^                         ┃^^                  ┃
┃ ^^                         ┃^^                  ┃
┃ ^^                         ┃^^                  ┃
┗^^━━━━━━━━━━━━━━━━━━━━━━━━━━┻^^━━━━━━━━━━━━━━━━━━┛
"
    ("n" mainspring-hydra-new-table :color red)
    ("N" mainspring-hydra-table-from-region :color red)
    ("s" mainspring-hydra-set-table-field :color red)
    ("<backtab>" org-table-previous-field :color red)
    ("<tab>" org-table-next-field :color red)
    ("c" org-table-insert-column :color red)
    ("r" org-table-insert-row :color red)
    ("d" kill-whole-line :color red)
    ("D" org-table-delete-column :color red)
    ("a" org-table-align :color red)
    ("q" hydra-pop :color blue))

  (defhydra mainspring-hydra-org-link (:hint nil)
    "
┏^^━━━━━━━━━━━━━━━━━━┓
┃^^ Org - Links      ┃
┣^^━━━━━━━━━━━━━━━━━━┫
┃ _h_: New HTTP Link ┃
┃ _f_: New File Link ┃
┃^^                  ┃
┃^^                  ┃
┃^^                  ┃
┃^^                  ┃
┃^^                  ┃
┃^^                  ┃
┃^^                  ┃
┗^^━━━━━━━━━━━━━━━━━━┛
"
    ("h" mainspring-hydra-insert-http-link :color blue)
    ("f" mainspring-hydra-insert-file-link :color blue)
    ("q" hydra-pop :color blue))

  (defhydra mainspring-hydra-org-formatting (:hint nil)
    "
┏^^━━━━━━━━━━━━━━━━━━┓
┃^^ Org - Formatting ┃
┣^^━━━━━━━━━━━━━━━━━━┫
┃ _b_: Bold          ┃
┃ _i_: Italic        ┃
┃ _u_: Underline     ┃
┃ _s_: Strikethrough ┃
┃^^                  ┃
┃^^                  ┃
┃^^                  ┃
┃^^                  ┃
┃^^                  ┃
┗^^━━━━━━━━━━━━━━━━━━┛
"
    ("b" mainspring-hydra-bold-region :color blue)
    ("i" mainspring-hydra-italic-region :color blue)
    ("u" mainspring-hydra-underline-region :color blue)
    ("s" mainspring-hydra-strikethrough-region :color blue)
    ("q" hydra-pop :color blue))

  (defun mainspring-hydra-insert-headline ()
    (interactive)
    (org-insert-heading)
    (org-edit-headline))

  (defun mainspring-hydra-delete-headline ()
    (interactive)
    (kill-whole-line)
    (delete-backward-char 1))

  (defun mainspring-hydra-priority-a ()
    (interactive)
    (org-priority ?A))

  (defun mainspring-hydra-priority-b ()
    (interactive)
    (org-priority ?B))

  (defun mainspring-hydra-priority-c ()
    (interactive)
    (org-priority ?C))

  (defun mainspring-hydra-priority-none ()
    (interactive)
    (org-priority ? ))

  (defun mainspring-hydra-status-todo ()
    (interactive)
    (org-todo "TODO"))

  (defun mainspring-hydra-status-done ()
    (interactive)
    (org-todo "DONE"))

  (defun mainspring-hydra-status-none ()
    (interactive)
    (org-todo ""))

  (defun mainspring-hydra-table-from-region (arg)
    (interactive "P")
    (org-table-create-or-convert-from-region arg)
    (org-table-insert-row)
    (org-table-insert-hline))

  (defun mainspring-hydra-new-table ()
    (interactive)
    (org-table-create)
    (org-table-next-field))

  (defun mainspring-hydra-set-table-field ()
    (interactive)
    (org-table-get-field nil (read-string "Edit: "))
    (org-table-align))

  (defun mainspring-hydra-insert-http-link (file-name)
    (interactive (list (read-string "URL: ")))
    (org-insert-link file-name file-name (read-string "Description: ")))

  (defun mainspring-hydra-insert-file-link (file-name)
    (interactive (list (read-file-name "File: ")))
    (org-insert-link file-name file-name (read-string "Description: ")))

  (defun mainspring-hydra-bold-region (beg end)
    (interactive "r")
    (wrap-region beg end "*"))

  (defun mainspring-hydra-italic-region (beg end)
    (interactive "r")
    (wrap-region beg end "/"))

  (defun mainspring-hydra-underline-region (beg end)
    (interactive "r")
    (wrap-region beg end "_"))

  (defun mainspring-hydra-strikethrough-region (beg end)
    (interactive "r")
    (wrap-region beg end "+"))

  ;;------------------------------------------------------------------------------

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
    ("n" (progn
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
    ("f" (progn
           (org-hydra-formatting/body)
           (hydra-push '(org-hydra-top/body))) "Formatting")
    ("k" (progn
           (org-hydra-latex/body)
           (hydra-push '(org-hydra-top/body))) "Latex")
    ("q" nil "Exit"))

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

  ;; Hydra for formatting related items.
  (defhydra org-hydra-formatting (:color blue :columns 4)
    "Org-mode formatting"
    ("b" org-bold-region "Bold")
    ("i" org-italic-region "Italic")
    ("u" org-underline-region "Underline")
    ("s" org-strikethrough-region "Strike through")
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
    ("s" org-table-from-selection "Table from selection")
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
    ("m" org-md-export-as-markdown "Export as markdown")
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

  ;; Hydra for org-mode tag related items.
  (defhydra org-hydra-latex (:color blue :columns 4)
    "Org-mode Latex"
    ("t" org-toggle-latex-fragment "Toggle Latex fragment")
    ("q" hydra-pop "Exit"))

  ;; Clojure Hydra
  (defhydra clojure-hydra (:color blue :columns 4)
    "Clojure"
    ("k" cider-quit "Close the REPL")
    ("l" cider-load-buffer "Load buffer in REPL")
    ("n" cider-repl-set-ns "Set REPL namespace")
    ("r" cider-jack-in "Launch REPL")
    ("s" cider-eval-last-sexp "Evaluate sexp")
    ("c" (lambda () (interactive) (cider-find-and-clear-repl-output t)) nil)
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

  (defhydra octave-hydra (:color blue :columns 4)
    "Octave"
    ("r" run-octave "Start Octave REPL.")
    ("l" octave-source-file "Send file to Octave REPL.")
    ("c" octave-clear "Clear Octave REPL.")
    ("q" nil "Exit"))

  (defhydra deft-hydra (:color blue :columns 4)
    "Deft"
    ("d" deft-delete-file "Delete file.")
    ("e" deft-rename-file "Rename file.")
    ("a" deft-archive-file "Archive file.")
    ("n" deft-new-file "New File.")
    ("r" deft-refresh "Refresh.")
    ("q" nil "Exit"))

  (defhydra slate-hydra (:color blue :columns 4)
    "Slate"
    ("r" slate-refresh "Refresh.")
    ("q" nil "Exit"))

  (defhydra calc-hydra (:color blue :columns 4)
    "Calc"
    ("p" calc-realign "Go to prompt")
    ("z" calc-undo "Undo")
    ("u" calc-redo "Redo")
    ("h" (progn
           (calc-describe-hydra/body)
           (hydra-push '(calc-hydra/body))) "Describe")
    ("q" nil "Exit"))

  (defhydra calc-describe-hydra (:color blue :columns 4)
    "Calc describe"
    ("f" mainspring-counsel-calc-describe-function "Function")
    ("v" mainspring-counsel-calc-describe-variable "Variable")
    ("k" calc-describe-key "Key")
    ("q" hydra-pop "Exit"))

  (defun mainspring-counsel-calc-describe-function()
    (interactive)
    (ivy-read "Describe calc function: "
              (calc-help-index-entries "Function" "Command")
              :require-match t
              :sort t
              :action (lambda (selection)
                        (calc-describe-function selection))))

  (defun mainspring-counsel-calc-describe-variable()
    (interactive)
    (ivy-read "Describe calc variable: "
              (calc-help-index-entries "Variable")
              :require-match t
              :sort t
              :action (lambda (selection)
                        (calc-describe-variable selection))))

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

  (defun launch-browser-repl ()
    (interactive)
    (httpd-start)
    (impatient-mode)
    (shell-command-to-string "start http://localhost:8080/imp/"))

  (defun org-insert-src-block ()
    (interactive)
    (ivy-read "Source  block language: "
              '("sql" "dot" "clojure" "octave")
              :require-match t
              :sort t
              :action (lambda (src-code-type)
                        (cond ((equal src-code-type "dot")
                               (progn
                                 (insert (format "#+BEGIN_SRC %s :file temp.png\n" src-code-type))
                                 (insert "digraph graphname {\n")
                                 (insert "  graph [bgcolor=\"#2b303b\", resolution=100, fontname=PragmataPro, fontcolor=\"#eff1f5\", fontsize=9];\n")
                                 (insert "  node [fontname=PragmataPro, fontcolor=\"#eff1f5\", color=\"#eff1f5\", fontsize=9];\n")
                                 (insert "  edge [fontname=PragmataPro, fontcolor=\"#eff1f5\", color=\"#eff1f5\", fontsize=9];\n")
                                 (insert "}\n")
                                 (newline-and-indent)
                                 (insert "#+END_SRC\n")
                                 (previous-line 2)
                                 (org-edit-src-code)))
                              (t
                               (progn
                                 (insert (format "#+BEGIN_SRC %s :results output\n" src-code-type))
                                 (newline-and-indent)
                                 (insert "#+END_SRC\n")
                                 (previous-line 2)
                                 (org-edit-src-code)))))))

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

(defun org-bold-region (beg end)
  (interactive "r")
  (wrap-region beg end "*"))

(defun org-italic-region (beg end)
  (interactive "r")
  (wrap-region beg end "/"))

(defun org-underline-region (beg end)
  (interactive "r")
  (wrap-region beg end "_"))

(defun org-strikethrough-region (beg end)
  (interactive "r")
  (wrap-region beg end "+"))

(defun wrap-region (beg end char)
  (interactive "r")
  (if (region-active-p)
      (progn
        (goto-char end)
        (insert char)
        (goto-char beg)
        (insert char))))

(defun org-table-from-selection (arg)
  (interactive "P")
  (org-table-create-or-convert-from-region arg)
  (org-table-insert-row)
  (org-table-insert-hline))

(defun octave-clear ()
  "Clear octave"
  (interactive)
  (let ((origin-buffer (current-buffer))
        (inhibit-read-only t))
    (switch-to-buffer (get-buffer "*Inferior Octave*"))
    (erase-buffer)
    (comint-send-input)
    (switch-to-buffer origin-buffer)))

(provide 'init-hydra)
