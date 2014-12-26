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