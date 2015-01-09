;; Use undo tree for better undo/redo support.
(require 'undo-tree)
(global-undo-tree-mode 1)

;; Allow multi-cursor editing.
(require 'multiple-cursors)

;; Duplicate an entire line.
(defun duplicate-line()
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (next-line 1)
  (yank))

;; Toggle the comment status of the current line. 
(defun toggle-comment-on-line ()
  "comment or uncomment current line"
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))

;; Move line up
(defun move-line-up ()
  (interactive)
  (transpose-lines 1)
  (previous-line 2))

;; Move line down
(defun move-line-down ()
  (interactive)
  (next-line 1)
  (transpose-lines 1)
  (previous-line 1))

;; Move a region x spaces over.
(defun shift-region (distance)
  (let ((mark (mark)))
    (save-excursion
      (indent-rigidly (region-beginning) (region-end) distance)
      (push-mark mark t t)
      ;; Tell the command loop not to deactivate the mark
      ;; for transient mark mode
      (setq deactivate-mark nil))))

;; Move a region 2 spaces right.
(defun shift-right ()
  (interactive)
  (shift-region 2))

;; Move a region 2 spaces left.
(defun shift-left ()
  (interactive)
  (shift-region -2))

;; No tabs, use spaces instead.
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)

;; Expand region support.
(require 'expand-region)

;; Ace jump support.
(require 'ace-jump-mode)

;; Highlight matching parens.
(show-paren-mode 1)

;; Allow up and down casing regions.
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; Delete a line
(defun delete-whole-line ()
  "Deletes a line, but does not put it in the kill-ring. (kinda)"
  (interactive)
  (move-beginning-of-line 1)
  (kill-line 1)
  (setq kill-ring (cdr kill-ring)))

;; Delete a sexp
(defun delete-sexp ()
  "Delete the sexp at point."
  (interactive)
  (cond
   ((paredit-in-comment-p)
    (call-interactively 'delete-char))
   ;; Strings don't behave the same as normal sexps in paredit.
   ((paredit-in-string-p)
    (delete-region (save-excursion (paredit-backward-up)
                                   (point))
                   (save-excursion (paredit-backward-up)
                                   (paredit-forward)
                                   (point))))
   ((paredit-inside-sexp-p)
    (delete-region (save-excursion (paredit-backward)
                                   (point))
                   (save-excursion (paredit-forward)
                                   (point))))
   ((paredit-start-of-sexp-p)
    (delete-region (point)
                   (save-excursion (paredit-forward)
                                   (point))))
   ;; Otherwise we're at the end of a sexp.
   (t
    (delete-region (save-excursion (paredit-backward)
                                   (point))
                   (save-excursion (paredit-backward)
                                   (paredit-forward)
                                   (point))))))

(defun paredit-inside-sexp-p ()
  "Are we inside the bounds of a sexp?"
  (= (save-excursion (paredit-forward)
                     (point))
     (save-excursion (paredit-backward)
                     (paredit-forward)
                     (point))))

(defun paredit-start-of-sexp-p ()
  "Are we at the start of a sexp?"
  (= (save-excursion (paredit-forward)
                     (paredit-backward)
                     (point))
     (point)))

;; Treat selections more like most editors do.
(delete-selection-mode 1)

;; Delete a word
(defun delete-word (arg)
  (interactive "p")
  (delete-region (point) (progn (forward-word arg) (point))))

;; Delete a word backwards.
(defun backward-delete-word (arg)
  (interactive "p")
  (delete-word (- arg)))
