;; -*- lexical-binding: t; -*-
(use-package bind-key
  :straight t

  :config
  ;; Leave no way to enable overwrite mode.
  (unbind-key "<insert>" global-map)

  (bind-keys*
   ;; Cancel with one press of escape instead of three. Also cancel out of many different interfaces.
   ("<escape>" . mainspring-keyboard-quit-all)

   ;; Quit an org source buffer if open.
   ("S-<escape>" . mainspring-quit-popup)

   ;; Save the current file.
   ("C-s" . save-buffer)

   ;; Save all files.
   ("C-S-s" . save-some-buffers)

   ;; Open or create a file.
   ("C-o" . find-file)

   ;; Close the current buffer.
   ("C-w" . kill-this-buffer)

   ;; Close Emacs.
   ("C-S-w" . save-buffers-kill-terminal)

   ;; Undo last action.
   ("C-z" . undo-fu-only-undo)

   ;; Redo last action.
   ("C-y" . undo-fu-only-redo)

   ;; Copy the current selection to the clipboard.
   ("C-c" . simpleclip-copy)

   ;; Cut the current selection to the clipboard.
   ("C-x" . simpleclip-cut)

   ;; cut the current line.
   ("C-S-x" . mainspring-cut-current-line)

   ;; Paste whatever is on the clipboard.
   ("C-v" . simpleclip-paste)

   ;; Paste whatever is n the kill ring.
   ("C-S-v" . yank)

   ;; Regex search in a file.
   ("C-f" . consult-line)

   ;; Regex search in a project.
   ("C-S-f" . consult-ripgrep)

   ;; List TODOs.
   ("C-S-t" . consult-todo)

   ;; List flycheck/flymake errors.
   ("C-S-m" . mainspring-show-errors)

   ;; Regex search and replace in file.
   ("C-h" . vr-replace-whole-buffer)

   ;; Seach and replace in project.
   ("C-S-h" . projectile-replace)

   ;; Jump to a line in a file file number.
   ("C-g" . goto-line)

   ;; Jump to a visible location.
   ("C-S-g" . avy-goto-char-timer)

   ;; Open a file in a project.
   ("C-p" . consult-projectile)

   ;; Change the active buffer for the window.
   ("C-b" . consult-buffer)

   ;; Execute any function.
   ("C-S-p" . execute-extended-command)

   ;; Close all other windows.
   ("M-!" . delete-other-windows)

   ;; Split the current window horizontally.
   ("M-@" . split-window-horizontally)

   ;; Split the current window vertically.
   ("M-#" . split-window-vertically)

   ;; Toggle the comment status of a line.
   ("C-/" . mainspring-toggle-comment)

   ;; Mark in rectangle mode.
   ("C-<return>" . rectangle-mark-mode)

   ;; Edit all lines that are marked.
   ("C-S-c" . iedit-rectangle-mode)

   ;; Edit everything matching selection.
   ("C-S-a" . iedit-mode)

   ;; Duplicate the current line.
   ("C-S-d" . mainspring-duplicate-line)

   ;; Move a line one line up.
   ("C-S-<up>" . move-text-up)

   ;; Move a line one line down.
   ("C-S-<down>" . move-text-down)

   ;; Delete a word.
   ("<C-backspace>" . mainspring-backward-delete-word)

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
   ("C-l". mainspring-select-current-line)

   ;; Move forward a symbol.
   ("C-<left>". sp-backward-symbol)

   ;; Move backward a symbol.
   ("C-<right>". sp-forward-symbol)

   ;; Move up five lines.
   ("C-<up>". mainspring-up-five-lines)

   ;; Move down five lines.
   ("C-<down>". mainspring-down-five-lines)

   ;; Move forward a sexp.
   ("C-S-<left>". sp-backward-sexp)

   ;; Move backward a sexp.
   ("C-S-<right>". sp-forward-sexp)

   ;; Launch general apps hydra.
   ("C-S-<tab>". mainspring-hydra-launch-apps)
   ("C-S-<iso-lefttab>" . mainspring-hydra-launch-apps)

   ;; Next window.
   ("M-<tab>". other-window)

   ;; Join lines.
   ("C-j". mainspring-join-lines)

   ;; Sort lines.
   ("<f9>" . sort-lines)

   ;; Move to beginning of line.
   ("<home>" . beginning-of-line)

   ;; Move to end of line.
   ("<end>" . end-of-line)

   ;; Move to beginning of buffer.
   ("C-<home>" . beginning-of-buffer)

   ;; Move to end of buffer.
   ("C-<end>" . end-of-buffer)

   ;; Popup a shell on the bottom of the screen.
   ("C-`" . shell-pop))

  (defun mainspring-duplicate-line()
    "Duplicate the current line."
    (interactive)
    (move-beginning-of-line 1)
    (kill-line)
    (yank)
    (open-line 1)
    (next-line 1)
    (yank))

  (defun mainspring-delete-word (arg)
    "Delete a word."
    (interactive "p")
    (delete-region (point) (progn (forward-word arg) (point))))

  (defun mainspring-backward-delete-word (arg)
    "Delete a word moving backwards."
    (interactive "p")
    (mainspring-delete-word (- arg)))

  (defun mainspring-keyboard-quit-all ()
    "Cancel many things."
    (interactive)
    (cond ((bound-and-true-p iedit-mode) (iedit-mode))
          ((bound-and-true-p iedit-rectangle-mode) (iedit-rectangle-mode))
          ((derived-mode-p 'eshell-mode) (eshell-interrupt-process))
          ((active-minibuffer-window) (keyboard-escape-quit))
          ((string-match ".*-popup.*" (buffer-name (window-buffer (minibuffer-selected-window)))) (magit-popup-quit))
          ((string-match "\\*docker\\*" (buffer-name (window-buffer (minibuffer-selected-window)))) (magit-popup-quit))
          (t (keyboard-quit))))

  (defun mainspring-quit-popup ()
    "Exit out of an editable popup like or source or with editor."
    (interactive)
    (cond ((bound-and-true-p org-src-mode) (org-edit-src-exit))
          ((bound-and-true-p with-editor-mode) (with-editor-cancel nil))
          ((equal major-mode 'help-mode) (kill-buffer))
          ((string-match ".* Export\*" (buffer-name (window-buffer (minibuffer-selected-window)))) (kill-buffer))
          ((string-match ".*docker-build-output.*" (buffer-name (window-buffer (minibuffer-selected-window)))) (kill-buffer))
          ((string-match "\\*run .*" (buffer-name (window-buffer (minibuffer-selected-window)))) (kill-buffer))
          ((string-match "\\*Org-Babel Error Output\\*" (buffer-name (window-buffer (minibuffer-selected-window)))) (kill-buffer))
          ((string-match "\\*HTTP Response\\*" (buffer-name (window-buffer (minibuffer-selected-window)))) (kill-buffer))))

  (defun mainspring-up-five-lines ()
    "Move up five lines."
    (interactive)
    (previous-line 5))

  (defun mainspring-down-five-lines ()
    "Move down five lines."
    (interactive)
    (next-line 5))

  (defun mainspring-hydra-launch-apps ()
    "Launch a apps hydra menu."
    (interactive)
    (mainspring-hydra-apps/body))

  (defun mainspring-is-region-active?? ()
    "Is there an active region?"
    (and mark-active
         (/= (point) (mark))))

  (defun mainspring-toggle-comment ()
    "Toggle the comment status of a line or a region."
    (interactive)
    (if (mainspring-is-region-active??)
        (comment-or-uncomment-region (region-beginning) (region-end))
      (comment-or-uncomment-region (line-beginning-position) (line-end-position))))

  ;; TODO: This seems to fail sometimes. Why?
  (defun mainspring-copy-clear-selection (beg end)
    "Copy with smartclip and then clear the selection."
    (interactive "r")
    (simpleclip-copy beg end)
    (keyboard-quit))

  (defun mainspring-select-current-line ()
    "Select the current line"
    (interactive)
    (beginning-of-line) ; move to end of line
    (set-mark (line-end-position)))

  (defun mainspring-join-lines (arg)
    "Join the current line and the line below."
    (interactive "p")
    (end-of-line)
    (delete-char 1)
    (delete-horizontal-space)
    (insert " "))

  (defun mainspring-cut-current-line ()
    "Cut the current line."
    (interactive)
    (mainspring-select-current-line)
    (simpleclip-cut (region-beginning) (region-end)))

  (defun mainspring-show-errors ()
    "Move down five lines."
    (interactive)
    (if (bound-and-true-p flycheck-mode)
        (consult-flycheck)
      (consult-flymake))))

(provide 'init-bind-key)
