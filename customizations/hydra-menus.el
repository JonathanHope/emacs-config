;; Contains all of the different hydra menus.

;; Apps Hydra
(defhydra apps-hydra (:color blue :columns 4)
  "Apps"
  ("o" org-mode-launch "Org-mode")
  ("m" org-export-dispatch "Magit")
  ("r" rainbow-mode "Rainbow Mode")
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
  ("q" nil "Exit"))