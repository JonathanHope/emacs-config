(use-package hydra
  :defer t
  :straight t

  :config
  ;; Settings.

  (setq hydra-posframe-show-params
        '(
          :internal-border-width 0
          :internal-border-color "red"
          :poshandler posframe-poshandler-window-center))

  ;; Support for nested Hydras

  (defvar mainspring-hydra-stack nil)

  (defun mainspring-hydra-push (expr)
    "Push a hydra onto the stack."
    (push `(lambda () ,expr) mainspring-hydra-stack))

  (defun mainspring-hydra-pop ()
    "Pop a hydra off the stack."
    (interactive)
    (let ((x (pop mainspring-hydra-stack)))
      (when x
        (funcall x))))

  ;; Apps Hydra

  (defhydra mainspring-hydra-apps (
                                   :hint nil
                                   :pre (setq hydra-hint-display-type 'posframe)
                                   :post (setq hydra-hint-display-type 'lv))
    "
┏^^━━━━━━━━━━━━┳^^━━━━━━━━━━━━━━━┳^^━━━━━━━━━━━━━┳^^━━━━━━━━━━━━━┳^^━━━━━━━━━━━━━━━━━┓
┃^^ Apps       ┃^^ Minor Modes   ┃^^ Describe    ┃^^ Whitespace  ┃^^ Zoom            ┃
┣^^━━━━━━━━━━━━╋^^━━━━━━━━━━━━━━━╋^^━━━━━━━━━━━━━╋^^━━━━━━━━━━━━━╋^^━━━━━━━━━━━━━━━━━┫
┃ _d_: Dired   ┃ _R_: Rainbow    ┃ _M_: Mode     ┃ _TAB_: Tabs   ┃ _+_: Zoom In      ┃
┃ _m_: Magit   ┃ _L_: Jinx       ┃ _F_: Function ┃ _SPC_: Spaces ┃ _-_: Zoom Out     ┃
┃ _r_: Regex   ┃ _S_: Smerge     ┃ _V_: Variable ┃ _._: Untabify ┃ _0_: Zoom Reset   ┃
┃ _s_: Eshell  ┃ _W_: Whitespace ┃ _A_: Face     ┃ _>_: Tabify   ┃^^                 ┃
┃ _n_: Deft    ┃ _C_: Flycheck   ┃^^             ┃^^             ┃^^                 ┃
┃ _t_: Slate   ┃^^               ┃^^             ┃^^             ┃^^                 ┃
┃ _c_: Calc    ┃^^               ┃^^             ┃^^             ┃^^                 ┃
┃ _i_: Ibuffer ┃^^               ┃^^             ┃^^             ┃^^                 ┃
┃ _e_: Notmuch ┃^^               ┃^^             ┃^^             ┃^^                 ┃
┗^^━━━━━━━━━━━━┻^^━━━━━━━━━━━━━━━┻^^━━━━━━━━━━━━━┻^^━━━━━━━━━━━━━┻^^━━━━━━━━━━━━━━━━━┛
┏^^━━━━━━━━━━━━━━━━━━━━━━━━━┳^^━━━━━━━━━━━━━━━━━━━━┳^^━━━━━━━━━━━━━━━━━━━━━━━┓
┃^^ Resize Window           ┃^^ Select Window      ┃^^ Manage Window         ┃
┣^^━━━━━━━━━━━━━━━━━━━━━━━━━╋^^━━━━━━━━━━━━━━━━━━━━╋^^━━━━━━━━━━━━━━━━━━━━━━━┫
┃ _<up>_: Spliter Up        ┃ _1_: Select Window 1 ┃ _|_: Split Horizontally ┃
┃ _<down>_: Spliter Down    ┃ _2_: Select Window 2 ┃ ___: Split Vertically   ┃
┃ _<left>_: Splitter Left   ┃ _3_: Select Window 3 ┃ _x_: Delete             ┃
┃ _<right>_: Splitter Right ┃ _4_: Select Window 4 ┃ _b_: Change Buffer      ┃
┃ _=_: Balance Windows      ┃^^                    ┃ _f_: Choose File        ┃
┗^^━━━━━━━━━━━━━━━━━━━━━━━━━┻^^━━━━━━━━━━━━━━━━━━━━┻^^━━━━━━━━━━━━━━━━━━━━━━━┛
"
    ("d" dired :color red)
    ("m" mainspring-hydra-apps-magit-status :color red)
    ("o" mainspring-hydra-apps-org-mode-launch :color red)
    ("s" eshell :color red)
    ("n" deft :color red)
    ("t" slate :color red)
    ("c" full-calc :color red)
    ("i" ibuffer :color red)
    ("e" notmuch :color red)
    ("L" jinx-mode :color red)
    ("R" rainbow-mode :color red)
    ("S" smerge-start-session :color red)
    ("C" flycheck-mode :color red)
    ("W" whitespace-mode :color red)
    ("M" describe-mode :color blue)
    ("F" describe-function :color blue)
    ("V" describe-variable :color blue)
    ("A" describe-face :color blue)
    ("TAB" mainspring-hydra-apps-enable-tabs :color red)
    ("SPC" mainspring-hydra-apps-disable-tabs :color red)
    ("." untabify :color blue)
    (">" tabify :color blue)
    ("r" mainspring-hydra-apps-re-builder :color red)
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
    ("<up>" mainspring-hydra-apps-move-splitter-up :color red)
    ("<down>" mainspring-hydra-apps-move-splitter-down :color red)
    ("<left>" mainspring-hydra-apps-move-splitter-left :color red)
    ("<right>" mainspring-hydra-apps-move-splitter-right :color red)
    ("b" consult-buffer :color red)
    ("f" find-file :color red)
    ("=" balance-windows :color red)
    ("q" nil :color blue))

  (defun mainspring-hydra-apps-magit-status ()
    "Don't split window."
    (interactive)
    (let ((pop-up-windows nil))
      (call-interactively 'magit-status)))

  (defun mainspring-hydra-apps-new-empty-buffer ()
    (interactive)
    (let (($buf (generate-new-buffer "untitled")))
      (switch-to-buffer $buf)
      $buf))

  (defun mainspring-hydra-apps-org-mode-launch ()
    "Launch org-mode in the correct directory."
    (interactive)
    (mainspring-hydra-apps-new-empty-buffer)
    (setq default-directory notes-directory)
    (org-mode))

  (defun mainspring-hydra-apps-re-builder ()
    (interactive)
    (mainspring-hydra-apps-new-empty-buffer)
    (re-builder)
    (reb-change-syntax 'pcre)
    (windmove-up))

  (defun mainspring-hydra-apps-move-splitter-left (arg)
    "Move window splitter left."
    (interactive "p")
    (if (let ((windmove-wrap-around))
          (windmove-find-other-window 'right))
        (shrink-window-horizontally arg)
      (enlarge-window-horizontally arg)))

  (defun mainspring-hydra-apps-move-splitter-right (arg)
    "Move window splitter right."
    (interactive "p")
    (if (let ((windmove-wrap-around))
          (windmove-find-other-window 'right))
        (enlarge-window-horizontally arg)
      (shrink-window-horizontally arg)))

  (defun mainspring-hydra-apps-move-splitter-up (arg)
    "Move window splitter up."
    (interactive "p")
    (if (let ((windmove-wrap-around))
          (windmove-find-other-window 'up))
        (enlarge-window arg)
      (shrink-window arg)))

  (defun mainspring-hydra-apps-move-splitter-down (arg)
    "Move window splitter down."
    (interactive "p")
    (if (let ((windmove-wrap-around))
          (windmove-find-other-window 'up))
        (shrink-window arg)
      (enlarge-window arg)))

  (defun mainspring-hydra-apps-disable-tabs (arg)
    (interactive "p")
    (setq indent-tabs-mode nil))

  (defun mainspring-hydra-apps-enable-tabs (arg)
    (interactive "p")
    (setq indent-tabs-mode t))

  (defun mainspring-hydra-apps-flycheck (arg)
    (interactive "p")
    (flycheck-languagetool-setup)
    (flycheck-mode))


  ;; Org-mode Hydras

  (defhydra mainspring-hydra-org (:hint nil)
    "
┏^^━━━━━━━━━━━━━━━━┓
┃^^ Org            ┃
┣^^━━━━━━━━━━━━━━━━┫
┃ _h_: Headlines   ┃
┃ _l_: Plain Lists ┃
┃ _t_: Tables      ┃
┃ _L_: Links       ┃
┃ _f_: Formatting  ┃
┃ _s_: Source      ┃
┃ _e_: Export      ┃
┃ _k_: Latex       ┃
┃ _v_: Visibility  ┃
┃ _p_: Paragraphs  ┃
┗^^━━━━━━━━━━━━━━━━┛
"
    ("h" (progn
           (mainspring-hydra-org-headline/body)
           (mainspring-hydra-push '(mainspring-hydra-org/body))) :color blue)
    ("l" (progn
           (mainspring-hydra-org-plain-list/body)
           (mainspring-hydra-push '(mainspring-hydra-org/body))) :color blue)
    ("t" (progn
           (mainspring-hydra-org-table/body)
           (mainspring-hydra-push '(mainspring-hydra-org/body))) :color blue)
    ("L" (progn
           (mainspring-hydra-org-link/body)
           (mainspring-hydra-push '(mainspring-hydra-org/body))) :color blue)
    ("f" (progn
           (mainspring-hydra-org-formatting/body)
           (mainspring-hydra-push '(mainspring-hydra-org/body))) :color blue)
    ("s" (progn
           (mainspring-hydra-org-source/body)
           (mainspring-hydra-push '(mainspring-hydra-org/body))) :color blue)
    ("e" (progn
           (mainspring-hydra-org-export/body)
           (mainspring-hydra-push '(mainspring-hydra-org/body))) :color blue)
    ("k" (progn
           (mainspring-hydra-org-latex/body)
           (mainspring-hydra-push '(mainspring-hydra-org/body))) :color blue)
    ("v" (progn
           (mainspring-hydra-org-visibility/body)
           (mainspring-hydra-push '(mainspring-hydra-org/body))) :color blue)
    ("p" (progn
           (mainspring-hydra-org-paragraph/body)
           (mainspring-hydra-push '(mainspring-hydra-org/body))) :color blue)
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
┃ _<down>_: Next Headline    ┃ _p_: Set Property  ┃
┗^^━━━━━━━━━━━━━━━━━━━━━━━━━━┻^^━━━━━━━━━━━━━━━━━━┛
"
    ("n" mainspring-hydra-org-insert-headline :color red)
    ("d" mainspring-hydra-org-delete-headline :color red)
    ("s" org-edit-headline :color red)
    ("<left>" org-do-promote :color red)
    ("<right>" org-do-demote :color red)
    ("M-<up>" org-move-subtree-up :color red)
    ("M-<down>" org-move-subtree-down :color red)
    ("<down>" outline-next-heading :color red)
    ("<up>" outline-previous-heading :color red)
    ("a" mainspring-hydra-org-priority-a :color red)
    ("b" mainspring-hydra-org-priority-b :color red)
    ("c" mainspring-hydra-org-priority-c :color red)
    ("r" mainspring-hydra-org-priority-none :color red)
    ("T" mainspring-hydra-org-status-todo :color red)
    ("D" mainspring-hydra-org-status-done :color red)
    ("R" mainspring-hydra-org-status-none :color red)
    ("t" org-set-tags-command :color red)
    ("p" org-set-property :color red)
    ("q" mainspring-hydra-pop :color blue))

  (defhydra mainspring-hydra-org-plain-list (:hint nil)
    "
┏^^━━━━━━━━━━━━━━━━━━━━━━━━━━━━━^^━━━━━━━━━━━━━━━━━━━━━┓
┃^^ Org - Plain Lists           ^^                     ┃
┣^^━━━━━━━━━━━━━━━━━━━━━━━━━━━━┳^^━━━━━━━━━━━━━━━━━━━━━┫
┃ _n_: New Unordered List Item ┃ _c_: Status CHECKED   ┃
┃ _N_: New Ordered List Item   ┃ _u_: Status UNCHECKED ┃
┃ _s_: Set List Item Text      ┃ _r_: Status NONE      ┃
┃ _<up>_: Previous List Item   ┃ _U_: Update Stats     ┃
┃ _<down>_: Next List Item     ┃^^                     ┃
┃ _<left>_: Outdent List Item  ┃^^                     ┃
┃ _<right>_: Indent List Item  ┃^^                     ┃
┃^^                            ┃^^                     ┃
┃^^                            ┃^^                     ┃
┗^^━━━━━━━━━━━━━━━━━━━━━━━━━━━━┻^^━━━━━━━━━━━━━━━━━━━━━┛
"
    ("n" mainspring-hydra-org-insert-plain-list-item-unordered :color red)
    ("N" mainspring-hydra-org-insert-plain-list-item-ordered :color red)
    ("s" mainspring-hydra-org-edit-plain-list-item :color red)
    ("<up>" org-previous-item :color red)
    ("<down>" org-next-item :color red)
    ("c" mainspring-hydra-org-list-item-checked :color red)
    ("u" mainspring-hydra-org-list-item-unchecked  :color red)
    ("r" mainspring-hydra-org-list-item-none  :color red)
    ("<left>" org-outdent-item  :color red)
    ("<right>" org-indent-item  :color red)
    ("U" mainspring-hydra-org-update-statistics  :color red)
    ("q" mainspring-hydra-pop :color blue))

  (defhydra mainspring-hydra-org-table (:hint nil)
    "
┏^^━━━━━━━━━━━━━━━━━━━━━━━━━━━^^━━━━━━━━━━━━━━━━━━┓
┃^^ Org - Tables              ^^                  ┃
┣^^━━━━━━━━━━━━━━━━━━━━━━━━━━┳^^━━━━━━━━━━━━━━━━━━┫
┃ _n_: New Table             ┃ _c_: New Column    ┃
┃ _N_: New Table from Data   ┃ _r_: New Row       ┃
┃ _s_: Set Field Text        ┃ _d_: Delete Row    ┃
┃ _<left>_: Left Cell        ┃ _D_: Delete Column ┃
┃ _<right>_: Right Cell      ┃ _a_: Align Table   ┃
┃^^                          ┃^^                  ┃
┃^^                          ┃^^                  ┃
┃^^                          ┃^^                  ┃
┃^^                          ┃^^                  ┃
┗^^━━━━━━━━━━━━━━━━━━━━━━━━━━┻^^━━━━━━━━━━━━━━━━━━┛
"
    ("n" mainspring-hydra-org-new-table :color red)
    ("N" mainspring-hydra-org-table-from-region :color red)
    ("s" mainspring-hydra-org-set-table-field :color red)
    ("<left>" org-table-previous-field :color red)
    ("<right>" org-table-next-field :color red)
    ("c" org-table-insert-column :color red)
    ("r" org-table-insert-row :color red)
    ("d" kill-whole-line :color red)
    ("D" org-table-delete-column :color red)
    ("a" org-table-align :color red)
    ("q" mainspring-hydra-pop :color blue))

  (defhydra mainspring-hydra-org-link (:hint nil)
    "
┏^^━━━━━━━━━━━━━━━━━━━┓
┃^^ Org - Links       ┃
┣^^━━━━━━━━━━━━━━━━━━━┫
┃ _h_: New HTTP Link  ┃
┃ _f_: New File Link  ┃
┃ _i_: New Image Link ┃
┃^^                   ┃
┃^^                   ┃
┃^^                   ┃
┃^^                   ┃
┃^^                   ┃
┃^^                   ┃
┗^^━━━━━━━━━━━━━━━━━━━┛
"
    ("h" mainspring-hydra-org-insert-http-link :color blue)
    ("f" mainspring-hydra-org-insert-file-link :color blue)
    ("i" mainspring-hydra-org-insert-image-link :color blue)
    ("q" mainspring-hydra-pop :color blue))

  (defhydra mainspring-hydra-org-formatting (:hint nil)
    "
┏^^━━━━━━━━━━━━━━━━━━━━━┓
┃^^ Org - Formatting    ┃
┣^^━━━━━━━━━━━━━━━━━━━━━┫
┃ _b_: Bold             ┃
┃ _i_: Italic           ┃
┃ _u_: Underline        ┃
┃ _s_: Strikethrough    ┃
┃ _c_: Code             ┃
┃ _d_: Remove Formating ┃
┃^^                     ┃
┃^^                     ┃
┃^^                     ┃
┗^^━━━━━━━━━━━━━━━━━━━━━┛
"
    ("b" mainspring-hydra-org-bold-region :color blue)
    ("i" mainspring-hydra-org-italic-region :color blue)
    ("u" mainspring-hydra-org-underline-region :color blue)
    ("c" mainspring-hydra-org-code-region :color blue)
    ("s" mainspring-hydra-org-strikethrough-region :color blue)
    ("d" mainspring-hydra-org-standard-region :color blue)
    ("q" mainspring-hydra-pop :color blue))

  (defhydra mainspring-hydra-org-source (:hint nil)
    "
┏^^━━━━━━━━━━━━━━━━━━━━━━━━━━┓
┃^^ Org - Source             ┃
┣^^━━━━━━━━━━━━━━━━━━━━━━━━━━┫
┃ _n_: New Source Block      ┃
┃ _s_: Set Source Block      ┃
┃ _e_: Execute Source Block  ┃
┃ _c_: Start Clojure Backend ┃
┃^^                          ┃
┃^^                          ┃
┃^^                          ┃
┃^^                          ┃
┃^^                          ┃
┗^^━━━━━━━━━━━━━━━━━━━━━━━━━━┛
"
    ("n" mainspring-hydra-org-insert-src-block :color blue)
    ("s" org-edit-src-code :color blue)
    ("e" org-babel-execute-src-block :color blue)
    ("c" mainspring-hydra-org-clojure-backend :color blue)
    ("q" mainspring-hydra-pop :color blue))

  (defhydra mainspring-hydra-org-export (:hint nil)
    "
┏^^━━━━━━━━━━━━━━━━━━━━━━━━━━┓
┃^^ Org - Export             ┃
┣^^━━━━━━━━━━━━━━━━━━━━━━━━━━┫
┃ _m_: Export as Markdown    ┃
┃ _r_: Export as Reveal      ┃
┃ _t_: Export as TaskJuggler ┃
┃^^                          ┃
┃^^                          ┃
┃^^                          ┃
┃^^                          ┃
┗^^━━━━━━━━━━━━━━━━━━━━━━━━━━┛
"
    ("m" org-pandoc-export-as-gfm :color blue)
    ("r" org-reveal-export-to-html :color blue)
    ("t" org-taskjuggler-export :color blue)
    ("q" mainspring-hydra-pop :color blue))

  (defhydra mainspring-hydra-org-latex (:hint nil)
    "
┏^^━━━━━━━━━━━━━━━━━━━━━━━━━━┓
┃^^ Org - Latex              ┃
┣^^━━━━━━━━━━━━━━━━━━━━━━━━━━┫
┃ _t_: Toggle Latex Fragment ┃
┃^^                          ┃
┃^^                          ┃
┃^^                          ┃
┃^^                          ┃
┃^^                          ┃
┃^^                          ┃
┃^^                          ┃
┃^^                          ┃
┗^^━━━━━━━━━━━━━━━━━━━━━━━━━━┛
"
    ("t" org-toggle-latex-fragment :color blue)
    ("q" mainspring-hydra-pop :color blue))

  (defhydra mainspring-hydra-org-visibility (:hint nil)
    "
┏^^━━━━━━━━━━━━━━━━━━━━━━━━━┓
┃^^ Org - Visibility        ┃
┣^^━━━━━━━━━━━━━━━━━━━━━━━━━┫
┃ _H_: Collapse All         ┃
┃ _S_: Expand All           ┃
┃ _<up>_: Previous Headline ┃
┃ _<down>_: Next Headline   ┃
┃ _h_: Hide                 ┃
┃ _s_: Show                 ┃
┃ _o_: Olivetti             ┃
┃^^                         ┃
┃^^                         ┃
┗^^━━━━━━━━━━━━━━━━━━━━━━━━━┛
"
    ("<up>" outline-previous-heading :color red)
    ("<down>" outline-next-heading :color red)
    ("H" outline-hide-body :color red)
    ("S" outline-show-all :color red)
    ("h" outline-hide-subtree :color red)
    ("s" outline-show-subtree :color red)
    ("o" olivetti-mode :color red)
    ("q" mainspring-hydra-pop :color blue))

  (defhydra mainspring-hydra-org-paragraph (:hint nil)
    "
┏^^━━━━━━━━━━━━━━━━━━━┓
┃^^ Org - Paragraphs  ┃
┣^^━━━━━━━━━━━━━━━━━━━┫
┃ _b_: Quote          ┃
┃^^                   ┃
┃^^                   ┃
┃^^                   ┃
┃^^                   ┃
┃^^                   ┃
┃^^                   ┃
┃^^                   ┃
┃^^                   ┃
┗^^━━━━━━━━━━━━━━━━━━━┛
"
    ("b" mainspring-hydra-org-insert-blockquote :color blue)
    ("q" mainspring-hydra-pop :color blue))

  (defun mainspring-hydra-org-insert-headline ()
    (interactive)
    (org-insert-heading))

  (defun mainspring-hydra-org-delete-headline ()
    (interactive)
    (kill-whole-line)
    (delete-backward-char 1))

  (defun mainspring-hydra-org-priority-a ()
    (interactive)
    (org-priority ?A))

  (defun mainspring-hydra-org-priority-b ()
    (interactive)
    (org-priority ?B))

  (defun mainspring-hydra-org-priority-c ()
    (interactive)
    (org-priority ?C))

  (defun mainspring-hydra-org-priority-none ()
    (interactive)
    (org-priority ?\s))

  (defun mainspring-hydra-org-status-todo ()
    (interactive)
    (org-todo "TODO"))

  (defun mainspring-hydra-org-status-done ()
    (interactive)
    (org-todo "DONE"))

  (defun mainspring-hydra-org-status-none ()
    (interactive)
    (org-todo ""))

  (defun mainspring-hydra-org-table-from-region (arg)
    (interactive "P")
    (org-table-create-or-convert-from-region arg)
    (org-table-insert-row)
    (org-table-insert-hline))

  (defun mainspring-hydra-org-new-table ()
    (interactive)
    (org-table-create)
    (org-table-next-field))

  (defun mainspring-hydra-org-set-table-field ()
    (interactive)
    (org-table-get-field nil (read-string "Edit: "))
    (org-table-align))

  (defun mainspring-hydra-org-insert-http-link (file-name)
    (interactive (list (read-string "URL: ")))
    (org-insert-link file-name file-name (read-string "Description: ")))

  (defun mainspring-hydra-org-insert-file-link (file-name)
    (interactive (list (read-file-name "File: ")))
    (org-insert-link file-name file-name (read-string "Description: ")))

(defun mainspring-hydra-org-insert-image-link (file-name)
    (interactive (list (read-file-name "File: ")))
    (progn
      (insert (format "[[file:%s]]" file-name))
      (org-redisplay-inline-images)))

  (defun mainspring-hydra-org-bold-region (beg end)
    (interactive "r")
    (org-emphasize ?*))

  (defun mainspring-hydra-org-italic-region (beg end)
    (interactive "r")
    (org-emphasize ?/))

  (defun mainspring-hydra-org-underline-region (beg end)
    (interactive "r")
    (org-emphasize ?_))

  (defun mainspring-hydra-org-strikethrough-region (beg end)
    (interactive "r")
    (org-emphasize ?+))

  (defun mainspring-hydra-org-standard-region (beg end)
    (interactive "r")
    (org-emphasize ?\s))

  (defun mainspring-hydra-org-code-region (beg end)
    (interactive "r")
    (org-emphasize ?~))

  (defun mainspring-hydra-org-insert-src-block (arg)
    (interactive
     (list
      (completing-read "Source block language: " '("sql"
                                                   "octave"
                                                   "mermaid"
                                                   "ebnf"
                                                   "xml"
                                                   "json"
                                                   "yaml"
                                                   "restclient"
                                                   "dockerfile"
                                                   "shell"
                                                   "terraform"
                                                   "graphql"
                                                   "typescript"
                                                   "tsx"
                                                   "html"
                                                   "css"
                                                   "gherkin"
                                                   "go"))))
    (cond ((equal arg "mermaid")
           (progn
             (insert (format "#+begin_src %s :file \"temp.svg\" :pupeteer-config-file \"~/.emacs.d/pupeteer-config.json\" :mermaid-config-file \"~/.emacs.d/mermaid-config.json\" :background-color \"transparent\"\n" arg))
             (newline-and-indent)
             (insert "#+end_src\n")
             (previous-line 2)
             (org-edit-src-code)))
          (t
           (progn
             (insert (format "#+begin_src %s\n" arg))
             (newline-and-indent)
             (insert "#+end_src\n")
             (previous-line 2)
             (org-edit-src-code)))))

  (defun mainspring-hydra-org-insert-plain-list-item (bullet)
    (interactive)
    (if (org-at-item-p)
        (progn
          (end-of-line)
          (org-insert-item)
          (next-line))
      (progn
        (message (thing-at-point 'line t))
        (insert (concat " " bullet " ")))))

  (defun mainspring-hydra-org-insert-plain-list-item-unordered ()
    (interactive)
    (mainspring-hydra-org-insert-plain-list-item "*"))

  (defun mainspring-hydra-org-insert-plain-list-item-ordered ()
    (interactive)
    (mainspring-hydra-org-insert-plain-list-item "1)"))

  (defun mainspring-hydra-org-edit-plain-list-item ()
    (interactive)
    (if (org-at-item-p)
        (let* ((input (read-string "Edit: "))
               (line (thing-at-point 'line t))
               (result (string-match " +\\(\\*\\|\\+\\|\\-\\|[0-9]+[\\)\\.]\\) \\(\\[ \\]\\|\\[X\\]\\)? ?" line))
               (match-end-index (+ 1 (nth (- (length (match-data)) 1) (match-data)))))
          (beginning-of-line)
          (forward-char match-end-index)
          (delete-region (point) (line-end-position))
          (insert input))
      (message "Not at item.")))

  (defun mainspring-hydra-org-list-item-status (status)
    (interactive)
    (let ((line (thing-at-point 'line t)))
      (if (string-match-p "\\(\\[ \\]\\|\\[X\\]\\) " line)
          (let* ((start-index (string-match "\\(\\[ \\]\\|\\[X\\]\\) " line)))
            (beginning-of-line)
            (forward-char start-index)
            (delete-char 4)
            (insert status)
            (org-update-statistics-cookies t))
        (let* ((start-index (string-match " +\\(\\*\\|\\+\\|\\-\\|[0-9]+[\\)\\.]\\) \\(\\[ \\]\\|\\[X\\]\\)? ?" line))
               (match-end-index (+ 1 (nth (- (length (match-data)) 1) (match-data)))))
          (beginning-of-line)
          (forward-char match-end-index)
          (insert status)
          (org-update-statistics-cookies t)))))

  (defun mainspring-hydra-org-list-item-checked ()
    (interactive)
    (mainspring-hydra-org-list-item-status "[X] "))

  (defun mainspring-hydra-org-list-item-unchecked ()
    (interactive)
    (mainspring-hydra-org-list-item-status "[ ] "))

  (defun mainspring-hydra-org-list-item-none ()
    (interactive)
    (mainspring-hydra-org-list-item-status ""))

  (defun mainspring-hydra-org-update-statistics ()
    (interactive)
    (org-update-statistics-cookies t))

  (defun mainspring-hydra-org-clojure-backend ()
    (interactive)
    (cider-jack-in nil))

  (defun mainspring-hydra-org-insert-blockquote()
    (interactive)
    (progn
      (insert "#+begin_quote\n")
      (newline-and-indent)
      (insert "#+end_quote\n")
      (previous-line 2)))

  ;; Clojure Hydra

  (defhydra mainspring-hydra-clojure (:hint nil)
    "
┏^^━━━━━━━━━━━━━━━━━━━━━━━━━━┓
┃^^ Clojure                  ┃
┣^^━━━━━━━━━━━━━━━━━━━━━━━━━━┫
┃ _o_: Launch CLJ REPL       ┃
┃ _j_: Launch CLJ REPL       ┃
┃ _J_: Launch CLJS REPL      ┃
┃ _l_: Load Buffer Into REPL ┃
┃ _n_: Set REPL Namespace    ┃
┃ _c_: Clear REPL            ┃
┃ _r_: Refresh REPL          ┃
┃ _k_: Close the REPL        ┃
┃ _f_: Evaluate Form Inline  ┃
┃ _F_: Evaluate Form         ┃
┗^^━━━━━━━━━━━━━━━━━━━━━━━━━━┛
"
    ("o" mainspring-hydra-clojure-launch-clj-repl :color blue)
    ("j" mainspring-hydra-clojure-join-clj-repl :color blue)
    ("J" mainspring-hydra-clojure-join-cljs-repl :color blue)
    ("l" cider-load-buffer :color blue)
    ("n" cider-repl-set-ns :color blue)
    ("c" mainspring-hydra-clojure-clear-repl :color blue)
    ("r" cider-ns-refresh :color blue)
    ("k" cider-quit :color blue)
    ("f" cider-eval-defun-at-point :color blue)
    ("F" cider-pprint-eval-defun-at-point :color blue)
    ("q" nil :color blue))

  (defhydra mainspring-hydra-clojure-cider (:hint nil)
    "
┏^^━━━━━━━━━━━━━━━━━━━━━━━━━━┓
┃^^ Clojure - Cider          ┃
┣^^━━━━━━━━━━━━━━━━━━━━━━━━━━┫
┃ _c_: Clear REPL            ┃
┗^^━━━━━━━━━━━━━━━━━━━━━━━━━━┛
"
    ("c" mainspring-hydra-clojure-clear-repl :color blue)
    ("q" nil :color blue))

  (defun mainspring-hydra-clojure-clear-repl ()
    (interactive)
    (cider-find-and-clear-repl-output t))

  (defun mainspring-hydra-clojure-launch-clj-repl ()
    (interactive)
    (add-hook 'cider-connected-hook 'mainspring-hydra-clojure-show-repl)
    (setq cider-repl-pop-to-buffer-on-connect t)
    (cider-jack-in nil))

  (defun mainspring-hydra-clojure-join-cljs-repl ()
    (interactive)
    (add-hook 'cider-connected-hook 'mainspring-hydra-clojure-show-repl)
    (setq cider-repl-pop-to-buffer-on-connect t)
    (setq )
    (cider-connect-cljs))

  (defun mainspring-hydra-clojure-join-clj-repl ()
    (interactive)
    (add-hook 'cider-connected-hook 'mainspring-hydra-clojure-show-repl)
    (setq cider-repl-pop-to-buffer-on-connect t)
    (cider-connect-clj))

  (defun mainspring-hydra-clojure-show-repl ()
    (interactive)
    (remove-hook 'cider-connected-hook 'mainspring-hydra-clojure-show-repl)
    (cider-find-and-clear-repl-output t)
    (balance-windows))

  (defhydra mainspring-hydra-dired (:hint nil)
    "
┏^^━━━━━━━━━━━━━━━━━^^━━━━━━━━━━━━━━━━━━━━━━━┓
┃^^ Dired           ^^                       ┃
┣^^━━━━━━━━━━━━━━━━┳^^━━━━━━━━━━━━━━━━━━━━━━━┫
┃ _<down>_: Next   ┃ _R_: Rename             ┃
┃ _<up>_: Previous ┃ _D_: Delete             ┃
┃ _<enter>_: Open  ┃ _C_: Copy               ┃
┃ _v_: View        ┃ _G_: Change Group       ┃
┃ _m_: Mark        ┃ _M_: Change Permissions ┃
┃ _u_: Unmark      ┃ _+_: Create Directory   ┃
┃ _U_: Unmark All  ┃ _<tab>_: Cycle Subtree  ┃
┃ _U_: Unmark All  ┃^^                       ┃
┃ _(_: Details     ┃^^                       ┃
┗^^━━━━━━━━━━━━━━━━┻^^━━━━━━━━━━━━━━━━━━━━━━━┛
"
    ("<down>" next-line :color red)
    ("<up>" previous-line :color red)
    ("<enter>" dired-find-file :color red)
    ("v" dired-display-file :color red)
    ("m" dired-mark :color red)
    ("u" dired-unmark :color red)
    ("U" dired-unmark-all-marks :color red)
    ("R" dired-do-rename :color red)
    ("D" dired-do-delete :color red)
    ("C" dired-do-copy :color red)
    ("+" dired-create-directory :color red)
    ("M" dired-do-chmod :color red)
    ("G" dired-do-chgrp :color red)
    ("(" dired-hide-details-mode :color red)
    ("<tab>" dired-subtree-toggle :color red)
    ("q" nil :color blue))

  ;; Deft Hydra

  (defhydra mainspring-hydra-deft (:hint nil)
    "
┏^^━━━━━━━━━━━━━━━━━━━━━┓
┃^^ Deft                ┃
┣^^━━━━━━━━━━━━━━━━━━━━━┫
┃ _<up>_: Previous File ┃
┃ _<down>_: Next File   ┃
┃ _D_: Delete File      ┃
┃ _R_: Rename File      ┃
┃ _A_: Archive File     ┃
┃ _N_: New File         ┃
┃ _r_: Refresh          ┃
┗^^━━━━━━━━━━━━━━━━━━━━━┛
"
    ("<down>" next-line :color red)
    ("<up>" previous-line :color red)
    ("D" deft-delete-file :color red)
    ("R" deft-rename-file :color red)
    ("A" deft-archive-file :color red)
    ("N" deft-new-file :color red)
    ("r" deft-refresh :color red)
    ("q" nil :color blue))

  ;; Slate Hydra

  (defhydra mainspring-hydra-slate (:hint nil)
    "
┏^^━━━━━━━━━━━━━━━━━━━━━┓
┃^^ Slate               ┃
┣^^━━━━━━━━━━━━━━━━━━━━━┫
┃ _r_: Refresh          ┃
┗^^━━━━━━━━━━━━━━━━━━━━━┛
"
    ("r" slate-refresh :color red)
    ("q" nil :color blue))

  ;; Calc Hydra

  (defhydra mainspring-hydra-calc (:hint nil)
    "
┏^^━━━━━━━━━━━━━━━━━━━━━━┓
┃^^ Calc                 ┃
┣^^━━━━━━━━━━━━━━━━━━━━━━┫
┃ _p_: Go to Prompt      ┃
┃ _z_: Undo              ┃
┃ _y_: Redo              ┃
┃ _v_: Paste             ┃
┃ _f_: Describe Function ┃
┃ _x_: Describe Variable ┃
┃ _k_: Describe Key      ┃
┗^^━━━━━━━━━━━━━━━━━━━━━━┛
"
    ("p" calc-realign :color red)
    ("z" calc-undo :color red)
    ("y" calc-redo :color red)
    ("v" mainspring-hydra-calc-paste :color red)
    ("f" mainspring-hydra-calc-describe-function :color blue)
    ("x" mainspring-hydra-calc-describe-variable :color blue)
    ("k" calc-describe-key :color blue)
    ("q" nil :color blue))

  (defun mainspring-hydra-calc-describe-function (arg)
    (interactive
     (list
      (completing-read "Describe calc function: " (calc-help-index-entries "Function" "Command"))))
    (calc-describe-function arg))

  (defun mainspring-hydra-calc-describe-variable (arg)
    (interactive
     (list
      (completing-read "Describe calc variable: " (calc-help-index-entries "Variable"))))
    (calc-describe-variable selection))

  (defun mainspring-hydra-calc-paste ()
    (interactive)
    (calc-wrapper
     (kill-new (simpleclip-get-contents))
     (calc-yank 0)))

  ;; Octave Hydra

  (defhydra mainspring-hydra-octave (:hint nil)
    "
┏^^━━━━━━━━━━━━━━━━━━━━━━━━━━┓
┃^^ Octave                   ┃
┣^^━━━━━━━━━━━━━━━━━━━━━━━━━━┫
┃ _o_: Launch REPL           ┃
┃ _k_: Close REPL            ┃
┃ _c_: Clear REPL            ┃
┃ _B_: Send Buffer to REPL   ┃
┃ _l_: Send Line to REPL     ┃
┃ _r_: Send Region to REPL   ┃
┃ _h_: Help                  ┃
┗^^━━━━━━━━━━━━━━━━━━━━━━━━━━┛
"
    ("o" mainspring-hydra-octave-launch-repl :color red)
    ("k" mainspring-hydra-octave-kill-repl :color red)
    ("c" mainspring-hydra-octave-clear-repl :color red)
    ("B" octave-send-buffer :color red)
    ("l" octave-send-line :color red)
    ("r" octave-send-region :color red)
    ("h" octave-help :color blue)
    ("q" nil :color blue))

  (defun mainspring-hydra-octave-clear-repl ()
    (interactive)
    (let ((origin-buffer (current-buffer))
          (inhibit-read-only t))
      (switch-to-buffer (get-buffer "*Inferior Octave*"))
      (erase-buffer)
      (comint-send-input)
      (comint-kill-whole-line -1)
      (switch-to-buffer origin-buffer)))

  (defun mainspring-hydra-octave-launch-repl ()
    (interactive)
    (run-octave)
    (mainspring-hydra-octave-clear-repl))

  (defun mainspring-hydra-octave-kill-repl ()
    (interactive)
    (octave-hide-process-buffer)
    (octave-kill-process))

  ;; Smerge Hydra

  (defhydra mainspring-hydra-smerge (:hint nil)
    "
┏^^━━━━━━━━━━━━━━━━━━━━━━━━━━┓
┃^^ Smerge                   ┃
┣^^━━━━━━━━━━━━━━━━━━━━━━━━━━┫
┃ _<up>_: Previous Item      ┃
┃ _<down>_: Next Item        ┃
┃ _m_: Keep Mine             ┃
┃ _t_: Keep Theirs           ┃
┃ _a_: Keep All              ┃
┗^^━━━━━━━━━━━━━━━━━━━━━━━━━━┛
"
    ("<up>" smerge-prev :color red)
    ("<down>" smerge-next :color red)
    ("<enter>" smerge-keep-current :color red)
    ("m" smerge-keep-mine :color red)
    ("t" smerge-keep-other :color red)
    ("a" smerge-keep-all :color red)
    ("q" nil :color blue))

  ;; Markdown Hydra

  (defhydra mainspring-hydra-markdown (:hint nil)
    "
┏^^━━━━━━━━━━━━━━━━━━━━━━━━━━┓
┃^^ Markdown                 ┃
┣^^━━━━━━━━━━━━━━━━━━━━━━━━━━┫
┃ _l_: Live Preview          ┃
┗^^━━━━━━━━━━━━━━━━━━━━━━━━━━┛
"
    ("l" markdown-live-preview-mode :color blue)
    ("q" nil :color blue))

  ;; restclient Hydra

  (defhydra mainspring-hydra-restclient (:hint nil)
    "
┏^^━━━━━━━━━━━━━━━━━━━━━━━━━━┓
┃^^ Restclient               ┃
┣^^━━━━━━━━━━━━━━━━━━━━━━━━━━┫
┃ _e_: Execute               ┃
┗^^━━━━━━━━━━━━━━━━━━━━━━━━━━┛
"
    ("e" restclient-http-send-current :color blue)
    ("q" nil :color blue)))

(provide 'init-hydra)

;; Ibuffer Hydra

(defhydra mainspring-hydra-ibuffer (:hint nil)
    "
┏^^━━━━━━━━━━━━━━━━━━━━━━━━━━┓
┃^^ Ibuffer                  ┃
┣^^━━━━━━━━━━━━━━━━━━━━━━━━━━┫
┃ _r_: Refresh               ┃
┃ _k_: Kill Buffer           ┃
┃ _<up>_: Previous           ┃
┃ _<down>_: Next             ┃
┗^^━━━━━━━━━━━━━━━━━━━━━━━━━━┛
"
    ("r" ibuffer-update :color red)
    ("k" ibuffer-do-delete :color red)
    ("<up>" previous-line :color red)
    ("<down>" next-line :color red)
    ("q" nil :color blue))

;; Notmuch hydra

(defhydra mainspring-hydra-notmuch-hello (:hint nil)
    "
┏^^━━━━━━━━━━━━━━━━━━━━━━━━━━┓
┃^^ Notmuch - Hello          ┃
┣^^━━━━━━━━━━━━━━━━━━━━━━━━━━┫
┃ _g_: Refresh               ┃
┃ _f_: Fetch                 ┃
┃ _s_: Search                ┃
┃ _i_: Inbox                 ┃
┃ _u_: Unread                ┃
┃ _m_: Compose Message       ┃
┗^^━━━━━━━━━━━━━━━━━━━━━━━━━━┛
"
    ("g" notmuch-refresh-this-buffer :color blue)
    ("f" mainspring-hydra-notmuch-fetch-mail :color blue)
    ("i" mainspring-hydra-notmuch-inbox :color blue)
    ("u" mainspring-hydra-notmuch-unread :color blue)
    ("s" notmuch-search :color blue)
    ("m" notmuch-mua-new-mail :color blue)
    ("q" nil :color blue))

(defhydra mainspring-hydra-notmuch-search (:hint nil)
    "
┏^^━━━━━━━━━━━━━━━━━━━━━━━━━━┓
┃^^ Notmuch - Search         ┃
┣^^━━━━━━━━━━━━━━━━━━━━━━━━━━┫
┃ _g_: Refresh               ┃
┗^^━━━━━━━━━━━━━━━━━━━━━━━━━━┛
"
    ("g" notmuch-refresh-this-buffer :color blue)
    ("q" nil :color blue))

(defhydra mainspring-hydra-notmuch-show (:hint nil)
    "
┏^^━━━━━━━━━━━━━━━━━━━━━━━━━━┓
┃^^ Notmuch - Show           ┃
┣^^━━━━━━━━━━━━━━━━━━━━━━━━━━┫
┃ _g_: Refresh               ┃
┃ _*_: Set Tags              ┃
┃ _a_: Archive               ┃
┃ _d_: Delete                ┃
┗^^━━━━━━━━━━━━━━━━━━━━━━━━━━┛
"
    ("g" notmuch-refresh-this-buffer :color blue)
    ("*" notmuch-show-tag :color blue)
    ("a" mainspring-hydra-notmuch-archive :color blue)
    ("d" mainspring-hydra-notmuch-delete :color blue)
    ("q" nil :color blue))


(defhydra mainspring-hydra-notmuch-message (:hint nil)
    "
┏^^━━━━━━━━━━━━━━━━━━━━━━━━━━┓
┃^^ Notmuch - Message        ┃
┣^^━━━━━━━━━━━━━━━━━━━━━━━━━━┫
┃ _s_: Send                  ┃
┃ _c_: Cancel                ┃
┗^^━━━━━━━━━━━━━━━━━━━━━━━━━━┛
"
    ("c" kill-this-buffer :color blue)
    ("s" mainspring-hydra-notmuch-send-messsage :color blue)
    ("q" nil :color blue))

(defun mainspring-hydra-notmuch-inbox ()
  (interactive)
  (notmuch-search "tag:inbox"))

(defun mainspring-hydra-notmuch-unread ()
  (interactive)
  (notmuch-search "tag:unread"))

(defun mainspring-hydra-notmuch-archive ( )
  (interactive)
  (notmuch-show-tag (list "-inbox")))

(defun mainspring-hydra-notmuch-delete ( )
  (interactive)
  (notmuch-show-tag (list "-inbox" "+deleted")))

(defun mainspring-hydra-notmuch-fetch-mail ( )
  (interactive)
  (shell-command "mbsync -a")
  (shell-command "notmuch new")
  (notmuch-refresh-this-buffer))

(defun mainspring-hydra-notmuch-send-messsage ( )
  (interactive)
  (message-send)
  (kill-this-buffer))

(defhydra mainspring-hydra-typescript (:hint nil)
    "
┏^^━━━━━━━━━━━━━━━━━━━━━━━━━━┓
┃^^ TypeScript               ┃
┣^^━━━━━━━━━━━━━━━━━━━━━━━━━━┫
┃ _r_: Rename                ┃
┃ _d_: Got to definition     ┃
┃ _u_: Find usages           ┃
┃ _c_: JSDoc                 ┃
┗^^━━━━━━━━━━━━━━━━━━━━━━━━━━┛
"
    ("r" eglot-rename :color blue)
    ("d" xref-find-definitions :color blue)
    ("u" xref-find-references :color blue)
    ("c" jsdoc :color blue)
    ("q" nil :color blue))

(defhydra mainspring-hydra-go (:hint nil)
    "
┏^^━━━━━━━━━━━━━━━━━━━━━━━━━━┓
┃^^ Go                       ┃
┣^^━━━━━━━━━━━━━━━━━━━━━━━━━━┫
┃ _r_: Rename                ┃
┃ _d_: Got to definition     ┃
┃ _u_: Find usages           ┃
┗^^━━━━━━━━━━━━━━━━━━━━━━━━━━┛
"
    ("r" eglot-rename :color blue)
    ("d" xref-find-definitions :color blue)
    ("u" xref-find-references :color blue)
    ("q" nil :color blue))
