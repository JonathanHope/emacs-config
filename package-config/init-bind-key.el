;; Package configuration for bind-key

(use-package bind-key
  :ensure t

  :config
  ;; Leave no way to enable overwrite mode.
  (unbind-key "<insert>" global-map)

  (bind-keys*
   ;; Cancel with one press of escape instead of three. Also cancel out of many different interfaces.
   ("<escape>" . keyboard-quit-all)

   ;; Quit an org source buffer if open.
   ("S-<escape>" . quit-org-source)

   ;; Save the current file.
   ("C-s" . save-buffer)

   ;; Save all files.
   ("C-S-s" . save-some-buffers)

   ;; Open or create a file.
   ("C-o" . counsel-find-file)

   ;; Close the current buffer.
   ("C-w" . kill-this-buffer)

   ;; Close Emacs.
   ("C-S-w" . save-buffers-kill-terminal)

   ;; Undo last action.
   ("C-z" . undo-tree-undo)

   ;; Redo last action.
   ("C-y" . undo-tree-redo)

   ;; Copy the current selection to the clipboard.
   ("C-c" . copy-clear-selection)

   ;; Cut the current selection to the clipboard.
   ("C-x" . simpleclip-cut)

   ;; cut the current line.
   ("C-S-x" . cut-current-line)

   ;; Paste whatever is on the clipboard.
   ("C-v" . simpleclip-paste)

   ;; Paste whatever is n the kill ring.
   ("C-S-v" . yank)

   ;; Regex search in a file.
   ("C-f" . swiper)

   ;; Regex search in a project.
   ("C-S-f" . counsel-projectile-rg)

   ;; Regex search and replace in file.
   ("C-h" . vr-replace-whole-buffer)

   ;; Seach and replace in project.
   ("C-S-h" . projectile-replace)

   ;; Jump to a line in a file file number.
   ("C-g" . goto-line)

   ;; Open a file in a project.
   ("C-p" . counsel-projectile-find-file)

   ;; Change the active buffer for the window.
   ("C-b" . ivy-switch-buffer)

   ;; Execute any function.
   ("C-S-p" . counsel-M-x)

   ;; Close all other windows.
   ("M-!" . delete-other-windows)

   ;; Split the current window horizontally.
   ("M-@" . split-window-horizontally)

   ;; Split the current window vertically.
   ("M-#" . split-window-vertically)

   ;; Toggle the comment status of a line.
   ("C-/" . toggle-comment)

   ;; Mark in rectangle mode.
   ("C-<return>" . rectangle-mark-mode)

   ;; Edit all lines that are marked.
   ("C-S-c" . iedit-rectangle-mode)

   ;; Edit everything matching selection.
   ("C-S-a" . iedit-mode)

   ;; Duplicate the current line.
   ("C-S-d" . duplicate-line)

   ;; Move a line one line up.
   ("C-S-<up>" . move-text-up)

   ;; Move a line one line down.
   ("C-S-<down>" . move-text-down)

   ;; Delete a word.
   ("<C-backspace>" . backward-delete-word)

   ;; Cut an entire line.
   ("C-k" . kill-whole-line)

   ;; Cut a sexp
   ("C-S-k" . sp-kill-sexp)

   ;; Disable the built in M-x function.
   ("M-x" . nil)

   ;; Expand the current selection one level.
   ("C-=" . er/expand-region)

   ;; Contract the current selection one level.
   ("C--" . er/contract-region)

   ;; Uppercase the current selection.
   ("C-S-u" . upcase-region)

   ;; Lowercase the current selection.
   ("C-S-l" . downcase-region)

   ;; Select an entire buffer.
   ("C-a" . mark-whole-buffer)

   ;; Select a word.
   ("C-d". mark-word)

   ;; Select a line.
   ("C-l". select-current-line)

   ;; Move forward a symbol.
   ("C-<left>". sp-backward-symbol)

   ;; Move backward a symbol.
   ("C-<right>". sp-forward-symbol)

   ;; Move up five lines.
   ("C-<up>". up-five)

   ;; Move down five lines.
   ("C-<down>". down-five)

   ;; Move forward a sexp.
   ("C-S-<left>". sp-backward-sexp)

   ;; Move backward a sexp.
   ("C-S-<right>". sp-forward-sexp)

   ;; Launch general apps hydra.
   ("C-S-<tab>". launch-hydra-apps)
   ("C-S-<iso-lefttab>" . launch-hydra-apps)

   ;; Join lines.
   ("C-j". join-lines)

   ;; Sort lines.
   ("<f9>" . sort-lines)

   ;; Jump to definition.
   ("<f12>" . dumb-jump-go)

   ;; Jump back.
   ("S-<f12>" . dumb-jump-back)

   ;; Move to beginning of line.
   ("S-<left>" . beginning-of-line)

   ;; Move to end of line.
   ("S-<right>" . end-of-line)

   ;; Move to beginning of buffer.
   ("<home>" . beginning-of-buffer)

   ;; Move to end of buffer.
   ("<end>" . end-of-buffer))

  (defun duplicate-line()
    "Duplicate the current line."
    (interactive)
    (move-beginning-of-line 1)
    (kill-line)
    (yank)
    (open-line 1)
    (next-line 1)
    (yank))

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
    (cond ((equal major-mode 'help-mode) (quit-window))
          ((bound-and-true-p iedit-mode) (iedit-mode))
          ((bound-and-true-p iedit-rectangle-mode) (iedit-rectangle-mode))
          ((active-minibuffer-window) (keyboard-escape-quit))
          ((string-match ".* Export\*" (buffer-name (window-buffer (minibuffer-selected-window)))) (quit-window))
          (t (keyboard-quit))))

  (defun quit-org-source ()
    "Exit out of an org source window."
    (interactive)
    (cond ((bound-and-true-p org-src-mode) (org-edit-src-exit))))

  (defun up-five ()
    "Move up five lines."
    (interactive)
    (previous-line 5))

  (defun down-five ()
    "Move down five lines."
    (interactive)
    (next-line 5))

  (defun launch-hydra-apps ()
    "Launch a apps hydra menu."
    (interactive)
    (mainspring-apps-hydra/body))

  (defun is-region-active? ()
    "Is there an active region?"
    (and mark-active
         (/= (point) (mark))))

  (defun toggle-comment ()
    "Toggle the comment status of a line or a region."
    (interactive)
    (if (is-region-active?)
        (comment-or-uncomment-region (region-beginning) (region-end))
      (comment-or-uncomment-region (line-beginning-position) (line-end-position))))

  (defun copy-clear-selection (beg end)
    "Copy with smartclip and then clear the selection."
    (interactive "r")
    (simpleclip-copy beg end)
    (keyboard-quit))

  (defun select-current-line ()
    "Select the current line"
    (interactive)
    (beginning-of-line) ; move to end of line
    (set-mark (line-end-position)))

  (defun join-lines (arg)
    (interactive "p")
    (end-of-line)
    (delete-char 1)
    (delete-horizontal-space)
    (insert " "))

  (defun cut-current-line ()
    (interactive)
    (select-current-line)
    (simpleclip-cut (region-beginning) (region-end))))

(provide 'init-bind-key)
