(deftheme base16-ocean-dark
  "")

(custom-theme-set-variables
  'base16-ocean-dark
  '(evil-normal-state-cursor   '("#8fa1b3" box))
  '(evil-insert-state-cursor   '("#bf616a"    (bar . 2)))
  '(evil-visual-state-curosr   '("#a3c6d0"   box))  ; FIXME: This doesn't seem to work for some reason.
  '(evil-replace-state-cursor  '("#dfe1e8" hbar))
  '(evil-emacs-state-cursor    '("#b48ead" box)))

(custom-theme-set-faces
  'base16-ocean-dark

  ;; Basics
  '(default     ((t (:foreground "#eff1f5" :background "#2b303b"))))
  '(bold        ((t (:weight bold))))
  '(bold-italic ((t (:slant italic :weight bold))))
  '(underline   ((t (:underline t))))
  '(italic      ((t (:slant italic))))
  '(shadow      ((t (:foreground "#4f5b66"))))
  '(success     ((t (:foreground "#a3be8c"))))
  '(error       ((t (:foreground "#bf616a"))))
  '(warning     ((t (:foreground "#d08770"))))

  ;; Outline
  '(outline-1 ((t (:foreground "#bf616a"))))
  '(outline-2 ((t (:foreground "#a3be8c"))))
  '(outline-3 ((t (:foreground "#d08770"))))
  '(outline-4 ((t (:foreground "#96b5b4"))))
  '(outline-5 ((t (:foreground "#ebcb8b"))))
  '(outline-6 ((t (:foreground "#8fa1b3"))))
  '(outline-7 ((t (:foreground "#d08770"))))
  '(outline-8 ((t (:foreground "#b48ead"))))

  ;; Font-lock stuff
  '(font-lock-builtin-face              ((t (:foreground "#96b5b4"))))
  '(font-lock-comment-delimiter-face    ((t (:foreground "#a7adba" :slant italic))))
  '(font-lock-comment-face              ((t (:foreground "#a7adba" :slant italic))))
  '(font-lock-constant-face             ((t (:foreground "#96b5b4"))))
  '(font-lock-doc-face                  ((t (:foreground "#a7adba"))))
  '(font-lock-doc-string-face           ((t (:foreground "#ebcb8b"))))
  '(font-lock-function-name-face        ((t (:foreground "#8fa1b3"))))
  '(font-lock-keyword-face              ((t (:foreground "#b48ead"))))
  '(font-lock-negation-char-face        ((t (:foreground "#a3be8c"))))
  '(font-lock-preprocessor-face         ((t (:foreground "#b48ead"))))
  '(font-lock-regexp-grouping-backslash ((t (:foreground "#ebcb8b"))))
  '(font-lock-regexp-grouping-construct ((t (:foreground "#b48ead"))))
  '(font-lock-string-face               ((t (:foreground "#a3be8c"))))
  '(font-lock-type-face                 ((t (:foreground "#ebcb8b"))))
  '(font-lock-variable-name-face        ((t (:foreground "#d08770"))))
  '(font-lock-warning-face              ((t (:foreground "#bf616a" :weight bold))))

  ;; Emacs interface
  '(cursor              ((t (:foreground "#8fa1b3" :background "#a3c6d0"))))
  '(fringe              ((t (:background "#343d46"))))
  '(linum               ((t (:foreground "#65737e" :background "#343d46"))))
  '(hl-line             ((t (:background "#343d46"))))
  '(border              ((t (:background "#343d46"))))
  '(border-glyph        ((t (:background "#343d46"))))
  '(highlight           ((t (:foreground "#2b303b" :background "#8fa1b3"))))
  '(link                ((t (:foreground "#8fa1b3" :underline t))))
  '(link-visited        ((t (:foreground "#b48ead" :underline t))))
  '(gui-element         ((t (:foreground "#eff1f5" :background "#343d46"))))
  '(minibuffer-prompt   ((t (:foreground "#a3c6d0"))))
  '(region              ((t (:foreground "#2b303b" :background "#8fa1b3"))))
  '(secondary-selection ((t (:foreground "#2b303b" :background "#a3c6d0"))))
  '(header-line         ((t (:inherit mode-line))))

  ;; Whitespace
  '(trailing-whitespace    ((t (:foreground "#a7adba" :background "#4f5b66"))))
  '(whitespace-empty       ((t (:foreground "#a7adba" :background "#4f5b66"))))
  '(whitespace-hspace      ((t (:foreground "#a7adba" :background "#4f5b66"))))
  '(whitespace-indentation ((t (:foreground "#a7adba" :background "#4f5b66"))))
  '(whitespace-line        ((t (:foreground "#a7adba" :background "#4f5b66"))))
  '(whitespace-newline     ((t (:foreground "#a7adba" :background "#4f5b66"))))
  '(whitespace-space       ((t (:foreground "#a7adba" :background "#4f5b66"))))
  '(whitespace-space       ((t (:foreground "#a7adba" :background "#4f5b66"))))
  '(whitespace-space       ((t (:foreground "#a7adba" :background "#4f5b66"))))
  '(whitespace-tab         ((t (:foreground "#a7adba" :background "#4f5b66"))))
  '(whitespace-trailing    ((t (:foreground "#a7adba" :background "#4f5b66"))))

  ;; Powerline & modeline
  '(mode-line           ((t (:foreground "#b48ead" :background "#343D46"   :box nil))))
  '(mode-line-inactive  ((t (:foreground "#4f5b66" :background "#343D46"   :box nil))))

  ;; Parentheses
  '(show-paren-match    ((t (:foreground "#eff1f5" :weight bold))))
  '(show-paren-mismatch ((t (:foreground "#eff1f5" :background "#bf616a" :weight bold))))

  '(rainbow-delimiters-depth-1-face   ((t (:foreground "#bf616a"))))
  '(rainbow-delimiters-depth-2-face   ((t (:foreground "#ebcb8b"))))
  '(rainbow-delimiters-depth-3-face   ((t (:foreground "#d08770"))))
  '(rainbow-delimiters-depth-4-face   ((t (:foreground "#a3be8c"))))
  '(rainbow-delimiters-depth-5-face   ((t (:foreground "#b48ead"))))
  '(rainbow-delimiters-depth-6-face   ((t (:foreground "#96b5b4"))))
  '(rainbow-delimiters-depth-7-face   ((t (:foreground "#ab7967"))))
  '(rainbow-delimiters-depth-8-face   ((t (:foreground "#8fa1b3"))))
  '(rainbow-delimiters-depth-9-face   ((t (:foreground "#a3c6d0"))))
  '(rainbow-delimiters-unmatched-face ((t (:foreground "#bf616a"))))

  ;; Search
  '(match                       ((t (:foreground "#2b303b" :background "#8fa1b3"))))
  '(isearch                     ((t (:foreground "#2b303b" :background "#8fa1b3"))))
  '(isearch-lazy-highlight-face ((t (:foreground "#2b303b" :background "#8fa1b3"))))
  '(isearch-fail                ((t (:foreground "#2b303b" :background "#bf616a"))))

  ;; IDO
  '(ido-subdir      ((t (:foreground "#a3be8c" :weight bold))))
  '(ido-first-match ((t (:foreground "#8fa1b3"))))
  '(ido-only-match  ((t (:foreground "#8fa1b3" :weight bold))))
  '(ido-indicator   ((t (:foreground "#8fa1b3"))))
  '(ido-virtual     ((t (:foreground "#ab7967"))))

  ;; Undo-tree
  '(undo-tree-visualizer-default-face        ((t (:foreground "#eff1f5"))))
  '(undo-tree-visualizer-current-face        ((t (:foreground "#2b303b" :background "#a3be8c"))))
  '(undo-tree-visualizer-active-branch-face  ((t (:foreground "#8fa1b3"))))
  '(undo-tree-visualizer-register-face       ((t (:foreground "#ebcb8b"))))
  '(undo-tree-visualizer-unmodified-face     ((t (:foreground "#b48ead"))))

  ;; diff-hl
  '(diff-hl-change  ((t (:foreground "#eff1f5" :background "#8fa1b3"))))
  '(diff-hl-insert  ((t (:foreground "#eff1f5" :background "#a3be8c"))))
  '(diff-hl-delete  ((t (:foreground "#eff1f5" :background "#bf616a"))))
  '(diff-hl-unknwon ((t (:foreground "#eff1f5" :background "#ebcb8b"))))

  ;; Magit
  '(magit-branch                       ((t (:foreground "#a3be8c" :weight bold))))
  '(magit-cherry-equivalent            ((t (:foreground "#96b5b4"))))
  '(magit-cherry-unmatched             ((t (:foreground "#bf616a"))))
  '(magit-diff-add                     ((t (:foreground "#2b303b" :background "#a3be8c"))))
  '(magit-diff-del                     ((t (:foreground "#2b303b" :background "#bf616a"))))
  '(magit-diff-file-header             ((t (:foreground "#2b303b" :background "#a3c6d0"))))
  '(magit-diff-hunk-header             ((t (:foreground "#2b303b" :background "#a3c6d0"))))
  '(magit-diff-merge-current           ((t (:foreground "#b48ead"))))
  '(magit-diff-merge-diff3-separator   ((t (:foreground "#b48ead"))))
  '(magit-diff-merge-proposed          ((t (:foreground "#ab7967"))))
  '(magit-diff-merge-separator         ((t (:foreground "#ab7967"))))
  '(magit-diff-none                    ((t (:slant italic))))
  '(magit-header                       ((t (:background "#343d46" :weight bold))))
  '(magit-item-highlight               ((t (:foreground "#2b303b" :background "#8fa1b3"))))
  '(magit-item-mark                    ((t (:foreground "#2b303b" :background "#8fa1b3"))))
  '(magit-key-mode-args-face           ((t (:foreground "#2b303b" :background "#dfe1e8"))))
  '(magit-key-mode-button-face         ((t (:foreground "#96b5b4"))))
  '(magit-key-mode-header-face         ((t (:foreground "#b48ead"))))
  '(magit-key-mode-switch-face         ((t (:foreground "#bf616a" :weight bold))))
  '(magit-log-author                   ((t (:foreground "#8fa1b3"))))
  '(magit-log-date                     ((t (:foreground "#b48ead"))))
  '(magit-log-graph                    ((t (:foreground "#eff1f5"))))
  '(magit-log-head-label-bisect-bad    ((t (:foreground "#2b303b" :background "#bf616a"))))
  '(magit-log-head-label-bisect-good   ((t (:foreground "#2b303b" :background "#a3be8c"))))
  '(magit-log-head-label-bisect-skip   ((t (:foreground "#2b303b" :background "#ebcb8b"))))
  '(magit-log-head-label-default       ((t (:foreground "#8fa1b3" :background "#2b303b" :box 1))))
  '(magit-log-head-label-head          ((t (:foreground "#d08770" :background "#2b303b" :box 1))))
  '(magit-log-head-label-local         ((t (:foreground "#b48ead" :background "#2b303b" :box 1))))
  '(magit-log-head-label-patches       ((t (:foreground "#bf616a" :background "#2b303b" :box 1))))
  '(magit-log-head-label-remote        ((t (:foreground "#a3be8c" :background "#2b303b" :box 1))))
  '(magit-log-head-label-tags          ((t (:foreground "#ebcb8b" :background "#2b303b" :box 1))))
  '(magit-log-head-wip                 ((t (:foreground "#96b5b4" :background "#2b303b" :box 1))))
  '(magit-log-reflog-label-amend       ((t (:foreground "#ebcb8b" :background "#2b303b" :box 1))))
  '(magit-log-reflog-label-checkout    ((t (:foreground "#96b5b4" :background "#2b303b" :box 1))))
  '(magit-log-reflog-label-cherry-pick ((t (:foreground "#a3be8c" :background "#2b303b" :box 1))))
  '(magit-log-reflog-label-commit      ((t (:foreground "#8fa1b3" :background "#2b303b" :box 1))))
  '(magit-log-reflog-label-merge       ((t (:foreground "#ab7967" :background "#2b303b" :box 1))))
  '(magit-log-reflog-label-other       ((t (:foreground "#eff1f5" :background "#2b303b" :box 1))))
  '(magit-log-reflog-label-rebase      ((t (:foreground "#b48ead" :background "#2b303b" :box 1))))
  '(magit-log-reflog-label-remote      ((t (:foreground "#d08770" :background "#2b303b" :box 1))))
  '(magit-log-reflog-label-reset       ((t (:foreground "#bf616a" :background "#2b303b" :box 1))))
  '(magit-log-sha1                     ((t (:foreground "#bf616a" :background "#2b303b" :box 1))))
  '(magit-process-ng                   ((t (:foreground "#2b303b" :background "#bf616a" :weight bold))))
  '(magit-process-ok                   ((t (:foreground "#2b303b" :background "#a3be8c" :weight bold))))
  '(magit-section-title                ((t (:background "#343d46"))))
  '(magit-tag                          ((t (:foreground "#b48ead" :background "#2b303b" :box 1))))
  '(magit-valid-signature              ((t (:foreground "#a3be8c" :weight bold))))
  '(magit-whitespace-warning-face      ((t (:foreground "#a7adba" :background "#4f5b66"))))

  ;; Custom
  '(custom-button ((t (:foreground "#eff1f5" :background "#4f5b66" :box 1))))
  '(custom-button-mouse ((t (:foreground "#eff1f5" :background "#8fa1b3" :box 1))))
  '(custom-button-pressed ((t (:foreground "#eff1f5" :background "#a3c6d0" :box 1))))
  '(custom-face-tag ((t (:foreground "#8fa1b3" :weight bold))))

  ;; Org
  '(org-agenda-date         ((t (:foreground "#8fa1b3"))))
  '(org-agenda-date-today   ((t (:foreground "#96b5b4" :weight bold))))
  '(org-agenda-date-weekend ((t (:foreground "#8fa1b3" :background "#343d46"))))
  '(org-agenda-structure    ((t (:foreground "#8fa1b3"))))
  '(org-agenda-done         ((t (:foreground "#a3be8c"))))
  '(org-date                ((t (:foreground "#8fa1b3" :underline t))))
  '(org-done                ((t (:foreground "#eff1f5"))))
  '(org-drawer              ((t (:foreground "#8fa1b3"))))
  '(org-ellipsis            ((t (:foreground "#eff1f5" :underline t))))
  '(org-footnote            ((t (:foreground "#96b5b4" :underline t))))
  '(org-hide                ((t (:foreground "#343d46"))))
  '(org-latex-and-related   ((t (:foreground "#d08770"))))
  '(org-list-dt             ((t (:foreground "#dfe1e8" :weight bold))))
  '(org-scheduled           ((t (:foreground "#a3be8c"))))
  '(org-scheduled-today     ((t (:foreground "#a3be8c" :weight bold))))
  '(org-table               ((t (:foreground "#d08770"))))
  '(org-todo                ((t (:foreground "#eff1f5"))))
  '(org-level-1             ((t (:foreground "#a3be8c" :weight bold))))
  '(org-level-2             ((t (:foreground "#a3be8c" :weight bold))))
  '(org-level-3             ((t (:foreground "#a3be8c" :weight bold))))
  '(org-level-4             ((t (:foreground "#a3be8c" :weight bold))))
  '(org-level-5             ((t (:foreground "#a3be8c" :weight bold))))
  '(org-level-6             ((t (:foreground "#a3be8c" :weight bold))))
  '(org-tag                 ((t (:foreground "#ebcb8b" :weight bold))))


  ;; Flyspell
  '(flyspell-duplicate ((t (:foreground "#eff1f5" :weight normal :underline (:color "#ebcb8b" :style wave)))))
  '(flyspell-incorrect ((t (:foreground "#eff1f5" :weight normal :underline (:color "#bf616a" :style wave)))))

  ;; multiple-cursors
  '(mc/cursor-face ((t (:foreground "#2b303b" :background "#8fa1b3" :inverse-video nil))))

  ;; idle-highlight
  `(idle-highlight ((t (:foreground "#eff1f5" :background "#343d46" :inverse-video nil))))

  ;; NeoTree
  `(neo-button-face ((t (:foreground "#8fa1b3" :background "#2b303b"))))
  `(neo-header-face ((t (:foreground "#b48ead" :background "#2b303b"))))
  `(neo-file-link-face ((t (:foreground "#eff1f5" :background "#2b303b")))))


(provide-theme 'base16-ocean-dark)

;; Helm
(custom-set-faces
 '(helm-M-x-key ((t (:foreground "#7FB37B"))))
 '(helm-buffer-directory ((t (:foreground "#A6ACB6"))))
 '(helm-buffer-file ((t nil)))
 '(helm-buffer-not-saved ((t (:foreground "#AA7961"))))
 '(helm-buffer-process ((t (:foreground "#7FB37B"))))
 '(helm-buffer-saved-out ((t nil)))
 '(helm-buffer-size ((t nil)))
 '(helm-candidate-number ((t (:background "#343D46" :foreground "#b48ead"))))
 '(helm-ff-directory ((t (:foreground "#A6ACB6"))))
 '(helm-ff-executable ((t (:foreground "#7FB37B"))))
 '(helm-ff-file ((t nil)))
 '(helm-ff-invalid-symlink ((t nil)))
 '(helm-ff-prefix ((t nil)))
 '(helm-ff-symlink ((t (:foreground "#AA7961"))))
 '(helm-grep-match ((t (:foreground "#AA7961"))))
 '(helm-grep-file ((t (:foreground "#7FB37B"))))
 '(helm-grep-lineno ((t (:foreground "#ebcb8b"))))
 '(helm-history-remote ((t nil)))
 '(helm-selection ((t (:background "#343D46"))))
 '(helm-source-header ((t (:background "#343D46" :foreground "#7FB37B"))))
 '(helm-visible-mark ((t nil)))
 '(helm-ff-dotted-directory ((t (:foreground "#A6ACB6")))))


;; Hide vertical boder
(set-face-attribute 'vertical-border
nil
:foreground "#2B303B")

;; Company
(custom-set-faces
 '(company-preview
   ((t (:background "#343D46" :foreground "#8FA1B3" :underline t))))
 '(company-preview-common
   ((t (:inherit company-preview))))
 '(company-tooltip
   ((t (:background "#8FA1B3" :foreground "#2B303B"))))
 '(company-scrollbar-fg
   ((t (:background "#65737E"))))
 '(company-scrollbar-bg
   ((t (:background "#8FA1B3"))))
 '(company-tooltip-selection
   ((t (:background "#65737E" :foreground "#2B303B"))))
 '(company-tooltip-common
   ((((type x)) (:inherit company-tooltip :weight bold))
    (t (:inherit company-tooltip))))
 '(company-tooltip-common-selection
   ((((type x)) (:inherit company-tooltip-selection :weight bold))
    (t (:inherit company-tooltip-selection)))))

;; Ace Jump Mode
(custom-set-faces
 '(ace-jump-face-foreground
   ((t (:foreground "#bf616a" :weight bold :underline t)))))

;; Smartparens
(custom-set-faces
 '(sp-pair-overlay-face
   ((t (:background "#2b303b"))))
 '(sp-wrap-overlay-face
   ((t (:background "#2b303b"))))
 '(sp-wrap-tag-overlay-face
   ((t (:background "#2b303b")))))

;; Multiple Cursors
(custom-set-faces 
 '(mc/cursor-bar-face
   ((t (:background "#2b303b" :foreground "#a3c6d0" :weight bold :height 1)))))

;; Helm swoop
(custom-set-faces 
 '(helm-swoop-target-line-face
   ((t (:background "#343d46"))))
 '(helm-swoop-target-line-block-face
   ((t (:background "#a3be8c"))))
 '(helm-swoop-target-word-face
   ((t (:background "#343d46" :weight bold :underline t)))))
