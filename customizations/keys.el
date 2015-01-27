;; Adding the key mappings to minor mode.
(defvar my-keys-minor-mode-map (make-keymap) "my-keys-minor-mode keymap.")

;; Cancel with one press of escape instead of three.
(define-key my-keys-minor-mode-map (kbd "<escape>") 'keyboard-quit)

;; More normal file shorctuts.
(define-key my-keys-minor-mode-map (kbd "C-s") 'save-buffer)
(define-key my-keys-minor-mode-map (kbd "C-o") 'helm-find-files)

;; More normal close shortcut.
(define-key my-keys-minor-mode-map (kbd "C-w") 'save-buffers-kill-emacs)

; ;; More normal search shortcuts.
(define-key my-keys-minor-mode-map (kbd "C-f") 'isearch-forward-regexp)
(define-key my-keys-minor-mode-map (kbd "C-S-f") 'isearch-backward-regexp)

;; More normal undo and redo shortcuts.
(define-key my-keys-minor-mode-map (kbd "C-z") 'undo-tree-undo)
(define-key my-keys-minor-mode-map (kbd "C-y") 'undo-tree-redo)

;; More normal copy and paste shorcuts.
(define-key my-keys-minor-mode-map (kbd "C-c") 'copy-region-as-kill)
(define-key my-keys-minor-mode-map (kbd "C-v") 'yank)
(define-key my-keys-minor-mode-map (kbd "C-x") 'kill-region)

;; Fuzzy buffer switching by default, ibuffer as secondary.
(define-key my-keys-minor-mode-map (kbd "C-b") 'helm-buffers-list)

;; Go to line.
(define-key my-keys-minor-mode-map (kbd "C-S-g") 'goto-line)

;; Fuzzy command matching.
(define-key my-keys-minor-mode-map (kbd "C-S-p") 'helm-M-x)

;; Go to file in project.
(define-key my-keys-minor-mode-map (kbd "C-p") 'helm-projectile-find-file)

;; Window management.
(define-key my-keys-minor-mode-map (kbd "<f1>") 'delete-other-windows)
(define-key my-keys-minor-mode-map (kbd "<f2>") 'split-window-horizontally)
(define-key my-keys-minor-mode-map (kbd "<f3>") 'split-window-vertically)

;; Open a terminal.
(define-key my-keys-minor-mode-map (kbd "<f4>") 'ansi-term)

;; Open the magit interface.
(define-key my-keys-minor-mode-map (kbd "<f6>") 'magit-status)

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
(define-key my-keys-minor-mode-map (kbd "C-S-r") 'duplicate-line)

;; Move line of text up or down.
(define-key my-keys-minor-mode-map (kbd "C-S-<up>") 'move-line-up)
(define-key my-keys-minor-mode-map (kbd "C-S-<down>") 'move-line-down)

;; Indent and unindent regions.
(define-key my-keys-minor-mode-map (kbd "<tab>") 'shift-right)
(define-key my-keys-minor-mode-map (kbd "<backtab>") 'shift-left)

;; Delete a region.
(define-key my-keys-minor-mode-map (kbd "<S-backspace>") 'delete-region)

;; Delete a word
(define-key my-keys-minor-mode-map (kbd "<C-backspace>") 'backward-delete-word)

;; Cut a line.
(define-key my-keys-minor-mode-map (kbd "C-k") 'kill-whole-line)

;; Delete a line.
(define-key my-keys-minor-mode-map (kbd "C-d") 'delete-whole-line)

;; Disable M-x
(define-key my-keys-minor-mode-map (kbd "M-x") nil)

;; Expand and contract a region.
(define-key my-keys-minor-mode-map (kbd "C-=") 'er/expand-region)
(define-key my-keys-minor-mode-map (kbd "C--") 'er/contract-region)

;;Ace jump mode.
(define-key my-keys-minor-mode-map (kbd "C-g") 'ace-jump-mode)

;; Change casing.
(define-key my-keys-minor-mode-map (kbd "C-S-u") 'upcase-region)
(define-key my-keys-minor-mode-map (kbd "C-S-l") 'downcase-region)

;; Command for commit that doesn't clash with anything else.
(define-key my-keys-minor-mode-map (kbd "M-c") 'git-commit-commit)

;; Select all.
(define-key my-keys-minor-mode-map (kbd "C-a") 'mark-whole-buffer)

;; Forward/backward word.
(define-key my-keys-minor-mode-map (kbd "C-<left>") 'backward-word)
(define-key my-keys-minor-mode-map (kbd "C-<right>") 'forward-word)

;; Jump many lines.
(define-key my-keys-minor-mode-map (kbd "C-<up>") (lambda () (interactive) (previous-line 5)))
(define-key my-keys-minor-mode-map (kbd "C-<down>") (lambda () (interactive) (next-line 5)))

;; Clojure Specific.

;; Cider history.
(define-key my-keys-minor-mode-map (kbd "M-<up>") 'cider-repl-backward-input)
(define-key my-keys-minor-mode-map (kbd "M-<down>") 'cider-repl-forward-input)

;; Cider compilation.
(define-key my-keys-minor-mode-map (kbd "M-x M-c") 'cider-load-buffer)
(define-key my-keys-minor-mode-map (kbd "M-x M-n") 'cider-repl-set-ns)
(define-key my-keys-minor-mode-map (kbd "M-x M-e") 'cider-eval-last-sexp)

;; Shortcut to start cider.
(define-key my-keys-minor-mode-map (kbd "<f5>") 'cider-jack-in)

;; Jump forward an sexp.
(define-key my-keys-minor-mode-map (kbd "C-S-<right>") 'forward-sexp)

;; Jump backward and sexp.
(define-key my-keys-minor-mode-map (kbd "C-S-<left>") 'backward-sexp)

;; Cut an sexp.
(define-key my-keys-minor-mode-map (kbd "C-S-k") 'kill-sexp)

;; Delete an sexp.
(define-key my-keys-minor-mode-map (kbd "C-S-d") 'delete-sexp)

;; Enable the minor mode with the key mappings.
(define-minor-mode my-keys-minor-mode t " my-keys" 'my-keys-minor-mode-map)
(my-keys-minor-mode 1)

;; Load my keys first.
(defadvice load (after give-my-keybindings-priority)
  "Try to ensure that my keybindings always have priority."
  (if (not (eq (car (car minor-mode-map-alist)) 'my-keys-minor-mode))
      (let ((mykeys (assq 'my-keys-minor-mode minor-mode-map-alist)))
        (assq-delete-all 'my-keys-minor-mode minor-mode-map-alist)
        (add-to-list 'minor-mode-map-alist mykeys))))
(ad-activate 'load)

;; Override other keymaps.

;; Override isearch forward/backward.
(define-key isearch-mode-map (kbd "<return>") 'isearch-repeat-forward)
(define-key isearch-mode-map (kbd "S-<return>") 'isearch-repeat-backward)

;; Tab for completion in helm windows.
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)

;; Tie escape to closing the autocomplete tooltip.
(eval-after-load "company"
  '(progn
    (define-key company-active-map (kbd "<escape>") 'company-abort)))

;; Tie escape to closing helm.
(eval-after-load "helm"
  '(progn
    (define-key helm-map (kbd "<escape>") 'helm-keyboard-quit)))
