;; Package configuration for custom-keymap.

;; This is a local package.
(load "custom-keymap.el")

(use-package custom-keymap
  :config 
  (configure-custom-keymap '(
    ;; Cancel with one press of escape instead of three. Also cancel out of many different interfaces.
    ("<escape>" keyboard-quit-all)

    ;; Save the current file.
    ("C-s" save-buffer)

    ;; Open or create a file.
    ("C-o" helm-find-files)

    ;; Close the current buffer.
    ("C-w" kill-this-buffer)

    ;; Close Emacs.
    ("C-S-w" save-buffers-kill-terminal)

    ;; Undo last action.
    ("C-z" undo-tree-undo)

    ;; Redo last action.
    ("C-y" undo-tree-redo)

    ;; Copy the current selection.
    ("C-c" copy-region-as-kill)

    ;; Cut the current selection.
    ("C-x" kill-region)

    ; Paste whatever is on the clipboard.
    ("C-v" yank)
   
    ;; Regex search in a file.
    ("C-f" isearch-forward-regexp)

    ;; Regex search in a project.
    ("C-S-f" helm-projectile-grep)

    ;; Jump to a line in a file.
    ("C-g" goto-line)

    ;; Jump anywhere visible.
    ("C-S-g" ace-jump-mode)

    ;; Open a file in a project.
    ("C-p" helm-projectile-find-file)

    ;; Change the active buffer for the window.
    ("C-b" helm-buffers-list)

    ;; Execute any function.
    ("C-S-p" helm-M-x)

    ;; Close all other windows.
    ("<f1>" delete-other-windows)

    ;; Split the current window horizontally.
    ("<f2>" split-window-horizontally)

    ;; Split the current window vertically.
    ("<f3>" split-window-vertically)

    ;; Toggle the comment status of a line.
    ("C-/" toggle-comment)

    ;; Mark in rectangle mode.
    ("C-<return>" rectangle-mark-mode)

    ;; Edit all lines that are marked.
    ("C-S-c" mc/edit-lines)

    ;; Add another cursor.
    ("C-<down-mouse-1>" mc/add-cursor-on-click)

    ;; Mark everything matching the selection.
    ("C-S-a" mc/mark-all-like-this)

    ;; Duplicate the current line.
    ("C-S-r" duplicate-line)

    ;; Move a line one line up.
    ("C-S-<up>" move-line-up)

    ;; Move a line one line down.
    ("C-S-<down>" move-line-down)

    ;; De;ete a word.
    ("<C-backspace>" backward-delete-word)

    ;; Cut an entire line.
    ("C-k" kill-whole-line)

    ;; Delete an entire line.
    ("C-d" delete-whole-line)

    ;; Disable the built in M-x function.
    ("M-x" nil)

    ;; Expand the current selection one level.
    ("C-=" er/expand-region)

    ;; Contract the current selection one level.
    ("C--" er/contract-region)

    ;; Uppercase the current selection.
    ("C-S-u" upcase-region)

    ;; Lowercase the current selection.
    ("C-S-u" downcase-region)

    ;; Select an entire buffer.
    ("C-a" mark-whole-buffer)

    ;; Move forward a word.
    ("C-<left>" backward-word)

    ;; Move backward a word.
    ("C-<right>" forward-word)

    ;; Move up ten lines.
    ("C-<up>" up-ten)

    ;; Move down ten lines.
    ("C-<down>" down-ten)

    ;; Launch a contextual keyboard driven menu.
    ("C-<tab>" launch-hydra-contextual)

    ;; Launch general apps hydra.
    ("C-S-<tab>" launch-hydra-apps)
    )))

(provide 'init-custom-keymap)