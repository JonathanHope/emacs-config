;; Disable the worthless suspend command.
(global-unset-key (kbd "C-z"))

;; Cancel with one press of escape instead of three.
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Override isearch forward/backward.
(define-key isearch-mode-map (kbd "<return>") 'isearch-repeat-forward)
(define-key isearch-mode-map (kbd "S-<return>") 'isearch-repeat-backward)

;; Assing the key mappings to minor mode.
(defvar my-keys-minor-mode-map (make-keymap) "my-keys-minor-mode keymap.")

;; More normal file shorctuts.
(define-key my-keys-minor-mode-map (kbd "C-s") 'save-buffer)
(define-key my-keys-minor-mode-map (kbd "C-o") 'find-file)

; ;; More normal search shortcuts.
(define-key my-keys-minor-mode-map (kbd "C-f") 'isearch-forward-regexp)
(define-key my-keys-minor-mode-map (kbd "C-S-f") 'isearch-backward-regexp)

;; More normal undo and redo shortcuts.
(define-key my-keys-minor-mode-map (kbd "C-z") 'undo-tree-undo)
(define-key my-keys-minor-mode-map (kbd "C-y") 'undo-tree-redo)

;; More normal copy and paste shorcuts.
(define-key my-keys-minor-mode-map (kbd "C-c") 'copy-region-as-kill)
(define-key my-keys-minor-mode-map (kbd "C-v") 'yank)

;; Fuzzy buffer switching by default, ibuffer as secondary.
(define-key my-keys-minor-mode-map (kbd "C-b") 'switch-to-buffer)
(define-key my-keys-minor-mode-map (kbd "C-x C-b") 'ibuffer)

;; Go to line.
(define-key my-keys-minor-mode-map (kbd "C-g") 'goto-line)

;; Fuzzy command matching.
(define-key my-keys-minor-mode-map (kbd "M-x") 'smex)

;; Go to file in project.
(define-key my-keys-minor-mode-map (kbd "C-p") 'projectile-find-file)

;; Window management.
(define-key my-keys-minor-mode-map (kbd "<f1>") 'delete-other-windows)
(define-key my-keys-minor-mode-map (kbd "<f2>") 'split-window-horizontally)
(define-key my-keys-minor-mode-map (kbd "<f3>") 'split-window-vertically)

;; Open a terminal.
(define-key my-keys-minor-mode-map (kbd "<f4>") 'ansi-term)

;; Comment toggling.
(define-key my-keys-minor-mode-map (kbd "C-/") 'toggle-comment-on-line)
(define-key my-keys-minor-mode-map (kbd "C-?") 'comment-or-uncomment-region)

;; Rectangle select.
(define-key my-keys-minor-mode-map (kbd "C-<return>") 'rectangle-mark-mode)

;; Allow multi cursor editing.
(define-key my-keys-minor-mode-map (kbd "C-S-c") 'mc/edit-lines)
(define-key my-keys-minor-mode-map (kbd "C-<down-mouse-1>") 'mc/add-cursor-on-click)
(define-key my-keys-minor-mode-map (kbd "C-S-a") 'mc/mark-all-like-this)

;; Duplicate a line.
(define-key my-keys-minor-mode-map (kbd "C-d") 'duplicate-line)

;; Move line of text up or down.
(define-key my-keys-minor-mode-map (kbd "C-S-<up>") 'move-line-up)
(define-key my-keys-minor-mode-map (kbd "C-S-<down>") 'move-line-down)

;; Indent and unindent regions.
(define-key my-keys-minor-mode-map (kbd "<tab>") 'shift-right)
(define-key my-keys-minor-mode-map (kbd "<backtab>") 'shift-left)

;; Enable the minor mode with the key mappings.
(define-minor-mode my-keys-minor-mode t " my-keys" 'my-keys-minor-mode-map)
(my-keys-minor-mode 1)