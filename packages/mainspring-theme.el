;;; mainspring-theme.el --- Custom color theme based on base16 ocean.

;; Copyright (C) 2018 Jonathan Hope

;; Author: Jonathan Hope <jonathan.douglas.hope@gmail.com>
;; Version: 1.0
;; Keywords: theme

;;; Commentary:

;; mainspring-theme is a theme based on base16 ocean.

;;; Code:

(deftheme mainspring
  "")

(custom-theme-set-faces
 'mainspring

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

 ;; Emacs interface
 '(cursor              ((t (:foreground "#eff1f5" :background "#a3c6d0"))))
 '(fringe              ((t (:background "#343d46"))))
 '(linum               ((t (:foreground "#65737e" :background "#2b303b" :weight normal :slant normal))))
 '(hl-line             ((t (:background "#4f5b66"))))
 '(border              ((t (:background "#343d46"))))
 '(border-glyph        ((t (:background "#343d46"))))
 '(highlight           ((t (:foreground "#2b303b" :background "#8fa1b3"))))
 '(link                ((t (:foreground "#8fa1b3" :underline t))))
 '(link-visited        ((t (:foreground "#b48ead" :underline t))))
 '(minibuffer-prompt   ((t (:foreground "#a3c6d0"))))
 '(region              ((t (:foreground "#2b303b" :background "#8fa1b3"))))
 '(secondary-selection ((t (:foreground "#2b303b" :background "#a3c6d0"))))
 '(header-line         ((t (:inherit mode-line))))

 ;; modeline
 '(mode-line           ((t (:foreground "#b48ead" :background "#343D46" :box nil))))
 '(mode-line-inactive  ((t (:foreground "#4f5b66" :background "#343D46" :box nil))))

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

 ;; diff-hl
 '(diff-hl-change  ((t (:background "#8fa1b3"))))
 '(diff-hl-insert  ((t (:background "#a3be8c"))))
 '(diff-hl-delete  ((t (:background "#bf616a"))))
 '(diff-hl-unknwon ((t (:background "#ebcb8b"))))

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
 '(magit-diff-added-highlight         ((t (:foreground "#a3be8c"))))
 '(magit-diff-removed-highlight       ((t (:foreground "#bf616a"))))
 '(magit-diff-added                   ((t (:foreground "#a3be8c"))))
 '(magit-diff-removed                 ((t (:foreground "#bf616a"))))
 '(magit-diffstat-removed             ((t (:foreground "#bf616a"))))
 '(magit-diffstat-added               ((t (:foreground "#a3be8c"))))
 '(magit-diff-context                 ((t (:foreground "#65737e"))))
 '(magit-diff-context-highlight       ((t (:foreground "#65737e"))))
 '(magit-diff-hunk-heading            ((t (:background "#4f5b66"))))
 '(magit-diff-hunk-heading-highlight  ((t (:background "#4f5b66"))))
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
 '(magit-section-highlight            ((t (:background "#4f5b66"))))
 '(magit-tag                          ((t (:foreground "#b48ead" :background "#2b303b" :box 1))))
 '(magit-valid-signature              ((t (:foreground "#a3be8c" :weight bold))))
 '(magit-whitespace-warning-face      ((t (:foreground "#a7adba"))))
 '(magit-branch-local                 ((t (:foreground "#96b5b4"))))
 '(magit-branch-remote                ((t (:foreground "#a3be8c"))))
 '(magit-branch-current               ((t (:foreground "#96b5b4"))))
 '(magit-section-heading              ((t (:foreground "#ebcb8b" :weight bold))))
 '(magit-hash                         ((t (:foreground "#4f5b66"))))

 ;; Org
 '(org-agenda-date         ((t (:foreground "#8fa1b3"))))
 '(org-agenda-date-today   ((t (:foreground "#96b5b4" :weight bold))))
 '(org-agenda-date-weekend ((t (:foreground "#8fa1b3" :background "#343d46"))))
 '(org-agenda-structure    ((t (:foreground "#8fa1b3"))))
 '(org-agenda-done         ((t (:foreground "#a3be8c"))))
 '(org-date                ((t (:foreground "#8fa1b3" :underline t))))
 '(org-done                ((t (:foreground "#96b5b4" :weight bold))))
 '(org-todo                ((t (:foreground "#d08770" :weight bold))))
 '(org-checkbox            ((t (:foreground "#c0c5ce" :weight bold))))
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
 '(org-block               ((t (:foreground "#eff1f5"))))
 '(org-block-begin-line    ((t (:background "#343d46" :foreground "#c0c5ce" :weight bold))))
 '(org-block-end-line      ((t (:background "#343d46" :foreground "#c0c5ce" :weight bold))))
 '(org-priority            ((t (:foreground "#ebcb8b" :weight bold))))
 '(org-special-keyword     ((t (:foreground "#65737e" :slant italic))))

 ;; Flyspell
 '(flyspell-duplicate ((t (:foreground "#ebcb8b" :underline t :weight bold))))
 '(flyspell-incorrect ((t (:foreground "#bf616a" :underline t :weight bold))))

 ;; multiple-cursors
 '(mc/cursor-face ((t (:foreground "#2b303b" :background "#8fa1b3" :inverse-video nil))))
 '(mc/cursor-bar-face ((t (:background "#2b303b" :foreground "#a3c6d0" :weight bold :height 1))))

 ;; Company
 '(company-preview ((t (:background "#343D46" :foreground "#8FA1B3" :underline t))))
 '(company-preview-common ((t (:inherit company-preview))))
 '(company-tooltip ((t (:background "#8FA1B3" :foreground "#2B303B"))))
 '(company-scrollbar-fg ((t (:background "#65737E"))))
 '(company-scrollbar-bg ((t (:background "#8FA1B3"))))
 '(company-tooltip-selection ((t (:background "#65737E" :foreground "#2B303B"))))
 '(company-tooltip-common ((((type x)) (:inherit company-tooltip :weight bold)) (t (:inherit company-tooltip))))
 '(company-tooltip-common-selection ((((type x)) (:inherit company-tooltip-selection :weight bold)) (t (:inherit company-tooltip-selection))))

 ;; Avy
 '(avy-lead-face  ((t (:foreground "#bf616a" :weight bold :underline t))))
 '(avy-lead-face-0 ((t (:foreground "#bf616a" :weight bold :underline t))))
 '(avy-lead-face-2 ((t (:foreground "#bf616a" :weight bold :underline t))))

 ;; Smartparens
 '(sp-pair-overlay-face     ((t (:background "#2b303b"))))
 '(sp-wrap-overlay-face     ((t (:background "#2b303b"))))
 '(sp-wrap-tag-overlay-face ((t (:background "#2b303b"))))

 ;; Visual regexp
 '(vr/match-0 ((t (:background "#343d46" :weight bold :underline t))))
 '(vr/match-1 ((t (:background "#343d46" :weight bold :underline t))))
 '(vr/group-0 ((t ())))
 '(vr/group-1 ((t ())))
 '(vr/group-2 ((t ())))

 ;; Mode line
 '(mainspring-mode-line-face               ((t (:foreground "#b48ead" :background "#343D46" :box nil))))
 '(mainspring-mode-line-inactive-face      ((t (:foreground "#4f5b66" :weight bold))))
 '(mainspring-mode-line-window-number-face ((t (:foreground "#ebcb8b" :weight bold))))
 '(mainspring-mode-line-file-status-face   ((t (:foreground "#d08770" :weight bold))))
 '(mainspring-mode-line-buffer-name-face   ((t (:foreground "#a3be8c" :weight bold))))
 '(mainspring-mode-line-projectile-face    ((t (:foreground "#8fa1b3" :weight bold))))
 '(mainspring-mode-line-mode-face          ((t (:foreground "#AA7961" :weight bold))))
 '(mainspring-mode-line-row-column-face    ((t (:foreground "#96b5b4" :weight bold))))
 '(mainspring-mode-line-scroll-bar-face    ((t (:foreground "#dfe1e8"))))

 ;; Web mode
 '(web-mode-html-tag-face         ((t (:foreground "#b48ead"))))
 '(web-mode-html-tag-bracket-face ((t (:foreground "#8fa1b3"))))
 '(web-mode-html-attr-name-face   ((t (:foreground "#ebcb8b"))))
 '(web-mode-html-attr-equal-face  ((t (:foreground "#d08770"))))
 '(web-mode-css-selector-face     ((t (:foreground "#ebcb8b"))))

 ;; Ivy
 '(ivy-current-match           ((t (:background "#343d46" :weight bold))))
 '(ivy-match-required-face     ((t (:foreground "#bf616a"))))
 '(ivy-minibuffer-match-face-1 ((t (:underline t :weight bold))))
 '(ivy-minibuffer-match-face-2 ((t (:underline t :weight bold))))
 '(ivy-minibuffer-match-face-3 ((t (:underline t :weight bold))))
 '(ivy-minibuffer-match-face-4 ((t (:underline t :weight bold))))
 '(swiper-match-face           ((t (:underline t :weight bold))))
 '(swiper-match-face-2         ((t (:underline t :weight bold))))

 ;; Highlight numbers
 '(highlight-numbers-number ((t (:foreground "#96b5b4" :slant italic))))

 ;; Default font locking
 '(font-lock-builtin-face              ((t (:foreground "#96b5b4"))))
 '(font-lock-comment-delimiter-face    ((t (:foreground "#65737e" :slant italic))))
 '(font-lock-comment-face              ((t (:foreground "#65737e" :slant italic))))
 '(font-lock-constant-face             ((t (:foreground "#96b5b4"))))
 '(font-lock-doc-face                  ((t (:foreground "#a7adba" :slant italic))))
 '(font-lock-doc-string-face           ((t (:foreground "#ebcb8b" :slant italic))))
 '(font-lock-function-name-face        ((t (:foreground "#8fa1b3"))))
 '(font-lock-keyword-face              ((t (:foreground "#b48ead"))))
 '(font-lock-negation-char-face        ((t (:foreground "#a3be8c"))))
 '(font-lock-preprocessor-face         ((t (:foreground "#b48ead"))))
 '(font-lock-regexp-grouping-backslash ((t (:foreground "#ebcb8b"))))
 '(font-lock-regexp-grouping-construct ((t (:foreground "#b48ead"))))
 '(font-lock-string-face               ((t (:foreground "#a3be8c" :slant italic))))
 '(font-lock-type-face                 ((t (:foreground "#ebcb8b"))))
 '(font-lock-variable-name-face        ((t (:foreground "#d08770"))))
 '(font-lock-warning-face              ((t (:foreground "#bf616a" :weight bold))))
 )

;; Hide vertical boder
(set-face-attribute 'vertical-border
                    nil
                    :foreground "#2B303B")

(provide-theme 'mainspring)

;; mainspring-theme.el ends here