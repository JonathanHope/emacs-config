;; Collection of custom functions used by hotkeys and the like.

(defun duplicate-line()
  "Duplicate the current line."
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (next-line 1)
  (yank))

(defun move-line-up ()
  "Move the current line up one line."
  (interactive)
  (transpose-lines 1)
  (previous-line 2))

(defun move-line-down ()
  "Move the current line down one line."
  (interactive)
  (next-line 1)
  (transpose-lines 1)
  (previous-line 1))

; (defun shift-region (distance)
;   "Shift an entire region by distance."
;   (let ((mark (mark)))
;     (save-excursion
;       (indent-rigidly (region-beginning) (region-end) distance)
;       (push-mark mark t t)
;       (setq deactivate-mark nil))))

; (defun shift-right ()
;   "Shift an entire region 2 spaces to the right."
;   (interactive)
;   (shift-region 2))

; (defun shift-left ()
;   "Shift an entire region 2 spaces to the left."
;   (interactive)
;   (shift-region -2))

(defun delete-whole-line ()
  "Delete the current line."
  (interactive)
  (move-beginning-of-line 1)
  (kill-line 1)
  (setq kill-ring (cdr kill-ring)))

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

(defun delete-sexp ()
  "Delete the current sexp."
  (interactive)
  (cond
   ((paredit-in-comment-p)
    (call-interactively 'delete-char))
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
   (t
    (delete-region (save-excursion (paredit-backward)
                                   (point))
                   (save-excursion (paredit-backward)
                                   (paredit-forward)
                                   (point))))))

(defun delete-word (arg)
  "Delete a word."
  (interactive "p")
  (delete-region (point) (progn (forward-word arg) (point))))

(defun backward-delete-word (arg)
  "Delete a word moving backwards."
  (interactive "p")
  (delete-word (- arg)))

(defun keyboard-quit-all ()
  "Quit out of whatever is currently going on."
  (interactive)
  (cond (helm-alive-p (helm-keyboard-quit))
        (t (keyboard-quit))))

(defun up-ten ()
  "Move up ten lines."
  (interactive) 
  (previous-line 10))

(defun down-ten ()
  "Move down ten lines."
  (interactive) 
  (next-line 10))

(defun launch-hydra-contextual ()
  "Launch a contextual hydra menu."
  (interactive)
  (cond 
    ((eq major-mode 'org-mode) (org-hydra-top/body))
    ((eq major-mode 'clojure-mode) (clojure-hydra/body))))

(defun launch-hydra-apps ()
  "Launch a apps hydra menu."
  (interactive)
  (apps-hydra/body))

(defun is-region-active? ()
  "Is there an active region?"
  (and mark-active
    (/= (point) (mark))))

(defun toggle-comment ()
  (interactive)
  (if (is-region-active?)
    (comment-or-uncomment-region (region-beginning) (region-end))
    (comment-or-uncomment-region (line-beginning-position) (line-end-position))))

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

(defun revert-default-directory ()
  "Revert the default directory to the directory that emacs was started in."
  (setq default-directory old-default-directory))

(defun startup ()
  "Custom startup function. Defaults to org mode in the notes directory."
  (make-directory notes-directory :parents)
  (if (not (file-exists-p (concat notes-directory ".projectile")))
    (write-region "" nil (concat notes-directory ".projectile"))
    nil)
  (setq default-directory notes-directory)
  (org-mode))

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