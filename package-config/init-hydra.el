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
┃^^                         ┃^^                    ┃ _f_: Choose File        ┃
┗^^━━━━━━━━━━━━━━━━━━━━━━━━━┻^^━━━━━━━━━━━━━━━━━━━━┻^^━━━━━━━━━━━━━━━━━━━━━━━┛
"
    ("d" dired :color red)
    ("m" mainspring-hydra-apps-magit-status :color red)
    ("o" mainspring-hydra-apps-org-mode-launch :color red)
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
    ("C" mainspring-hydra-apps-clojure-mode-launch :color red)
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
    ("b" ivy-switch-buffer :color red)
    ("f" counsel-find-file :color red)
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

  (defun mainspring-hydra-apps-clojure-mode-launch ()
    "Launch clojure-mode."
    (interactive)
    (mainspring-hydra-apps-new-empty-buffer)
    (clojure-mode))

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


  ;; Org-mode Hydras

  (defhydra mainspring-hydra-org (:hint nil)
    "
┏^^━━━━━━━━━━━━━━━┓
┃^^ Org           ┃
┣^^━━━━━━━━━━━━━━━┫
┃ _h_: Headlines  ┃
┃ _t_: Tables     ┃
┃ _l_: Links      ┃
┃ _f_: Formatting ┃
┃ _s_: Source     ┃
┃ _e_: Export     ┃
┃ _L_: Latex      ┃
┃ _v_: Visibility ┃
┃^^               ┃
┗^^━━━━━━━━━━━━━━━┛
"
    ("h" (progn
           (mainspring-hydra-org-headline/body)
           (mainspring-hydra-push '(mainspring-hydra-org/body))) :color blue)
    ("t" (progn
           (mainspring-hydra-org-table/body)
           (mainspring-hydra-push '(mainspring-hydra-org/body))) :color blue)
    ("l" (progn
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
    ("L" (progn
           (mainspring-hydra-org-latex/body)
           (mainspring-hydra-push '(mainspring-hydra-org/body))) :color blue)
    ("v" (progn
           (mainspring-hydra-org-visibility/body)
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
┃ _<down>_: Next Headline    ┃^^                  ┃
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
    ("q" mainspring-hydra-pop :color blue))

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
    ("n" mainspring-hydra-org-new-table :color red)
    ("N" mainspring-hydra-org-table-from-region :color red)
    ("s" mainspring-hydra-org-set-table-field :color red)
    ("<backtab>" org-table-previous-field :color red)
    ("<tab>" org-table-next-field :color red)
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
┃ _d_: Remove Formating ┃
┃^^                     ┃
┃^^                     ┃
┃^^                     ┃
┃^^                     ┃
┗^^━━━━━━━━━━━━━━━━━━━━━┛
"
    ("b" mainspring-hydra-org-bold-region :color blue)
    ("i" mainspring-hydra-org-italic-region :color blue)
    ("u" mainspring-hydra-org-underline-region :color blue)
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
    ("c" cider-jack-in :color blue)
    ("q" mainspring-hydra-pop :color blue))

  (defhydra mainspring-hydra-org-export (:hint nil)
    "
┏^^━━━━━━━━━━━━━━━━━━━━━━━┓
┃^^ Org - Export          ┃
┣^^━━━━━━━━━━━━━━━━━━━━━━━┫
┃ _m_: Export as Markdown ┃
┃^^                       ┃
┃^^                       ┃
┃^^                       ┃
┃^^                       ┃
┃^^                       ┃
┃^^                       ┃
┃^^                       ┃
┃^^                       ┃
┗^^━━━━━━━━━━━━━━━━━━━━━━━┛
"
    ("m" org-md-export-as-markdown :color blue)
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
┃ _n_: Narrow               ┃
┃ _w_: Widen                ┃
┃^^                         ┃
┗^^━━━━━━━━━━━━━━━━━━━━━━━━━┛
"
    ("<up>" outline-previous-heading :color red)
    ("<down>" outline-next-heading :color red)
    ("H" outline-hide-body :color red)
    ("S" outline-show-all :color red)
    ("n" org-narrow-to-subtree :color blue)
    ("w" widen :color blue)
    ("h" outline-hide-subtree :color red)
    ("s" outline-show-subtree :color red)
    ("q" mainspring-hydra-pop :color blue))

  (defun mainspring-hydra-org-insert-headline ()
    (interactive)
    (org-insert-heading)
    (org-edit-headline))

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

  (defun mainspring-hydra-org-insert-image-link ()
    (interactive)
    (ivy-read "File: "
              'read-file-name-internal
              :matcher #'counsel--find-file-matcher
              :require-match t
              :sort t
              :action (lambda (file-name)
                        (progn
                          (insert (format "[[file:%s]]" file-name))
                          (org-redisplay-inline-images)))))

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

  (defun mainspring-hydra-org-insert-src-block ()
    (interactive)
    (ivy-read "Source  block language: "
              '("sql" "dot" "clojure" "octave")
              :require-match t
              :sort t
              :action (lambda (src-code-type)
                        (cond ((equal src-code-type "dot")
                               (progn
                                 (newline-and-indent)
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
                                 (newline-and-indent)
                                 (insert (format "#+BEGIN_SRC %s :results output\n" src-code-type))
                                 (newline-and-indent)
                                 (insert "#+END_SRC\n")
                                 (previous-line 2)
                                 (org-edit-src-code)))))))

  ;; ------------------------ OLD ------------------------------------------------------------------------------------------------------------------------------

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
           (mainspring-hydra-push '(calc-hydra/body))) "Describe")
    ("q" nil "Exit"))

  (defhydra calc-describe-hydra (:color blue :columns 4)
    "Calc describe"
    ("f" mainspring-counsel-calc-describe-function "Function")
    ("v" mainspring-counsel-calc-describe-variable "Variable")
    ("k" calc-describe-key "Key")
    ("q" mainspring-hydra-pop "Exit"))

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
                        (calc-describe-variable selection)))))

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
