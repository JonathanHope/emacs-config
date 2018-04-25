;;; mainspring-org-prettify.el --- Grants the ability to theme org modes headlines and lists.

;; Copyright (C) 2018 Jonathan Hope

;; Author: Jonathan Hope <jonathan.douglas.hope@gmail.com>
;; Version: 1.0
;; Keywords: org, prettify

;;; Commentary:

;; mainspring-org-prettify grants the user the ability to safely theme headlines and plain
;; lists in org documents. The headlines will be indented headline level characters using
;; mainspring-org-prettify-headline-dash followed by mainspring-org-prettify-headline-bullet.
;; The plain list characters are simply swapped using mainspring-org-prettify-plain-list-plus-char,
;; mainspring-org-prettify-plain-list-asterisk-char, and mainspring-org-prettify-plain-list-minus-char.

;;; Code:

(eval-when-compile (require 'cl))

(defgroup mainspring-org-prettify nil
  "Visual tweaks for org mode."
  :group 'org-appearance)

(defcustom mainspring-org-prettify-headline-dash ?━
  "What character to use for indenting the org headline character."
  :group 'mainspring-org-prettify)

(defcustom mainspring-org-prettify-headline-bullet ?⬢
  "What character to demark a headline with."
  :group 'mainspring-org-prettify)

(defcustom mainspring-org-prettify-plain-list-plus-char ?➤
  "What character to demark a plain list using pluses with."
  :group 'mainspring-org-prettify)

(defcustom mainspring-org-prettify-plain-list-asterisk-char ?➤
  "What character to demark a plain list using asterisks with."
  :group 'mainspring-org-prettify)

(defcustom mainspring-org-prettify-plain-list-minus-char ?➤
  "What character to demark a plain list using minuses with."
  :group 'mainspring-org-prettify)

(defun mainspring-org-prettify-add-to-list (list-var element)
  "Adds an element to the end of a list."
  (set list-var
	     (append (symbol-value list-var) (list element)) ))

(defun mainspring-org-prettify-get-headline-char (level)
  (let ((headline-char '()))
    (progn
      (dotimes (number level)
        (progn
          (mainspring-org-prettify-add-to-list 'headline-char mainspring-org-prettify-headline-dash)
          (mainspring-org-prettify-add-to-list 'headline-char '(Br . Bl))))
      (mainspring-org-prettify-add-to-list 'headline-char mainspring-org-prettify-headline-bullet))))

(define-minor-mode mainspring-org-prettify-mode
  :group 'mainspring-org-prettify
  "Visual tweaks for org mode."
  nil nil nil
  (let* ((headlines
          `(("^\\*+ "
             (0 (let* (( level (- (match-end 0) (match-beginning 0) 1)))
                  (compose-region (- (match-end 0) (+ 2 (- level 1)))
                                  (- (match-end 0) 1)
                                  (mainspring-org-prettify-get-headline-char level))
                  nil)))))

         (plain-list-plus
          `(("^ +\\+ "
             (0
              (prog1 () (compose-region
                         (- (match-end 0) 2)
                         (- (match-end 0) 1)
                         mainspring-org-prettify-plain-list-plus-char))))))

         (plain-list-asterisk
          `(("^ +\\* "
             (0
              (prog1 () (compose-region
                         (- (match-end 0) 2)
                         (- (match-end 0) 1)
                         mainspring-org-prettify-plain-list-asterisk-char))))))

         (plain-list-minus
          `(("^ +\\- "
             (0
              (prog1 () (compose-region
                         (- (match-end 0) 2)
                         (- (match-end 0) 1)
                         mainspring-org-prettify-plain-list-minus-char)))))))

    (if mainspring-org-prettify-mode
        (progn
          (font-lock-add-keywords nil headlines)
          (font-lock-add-keywords nil plain-list-plus)
          (font-lock-add-keywords nil plain-list-asterisk)
          (font-lock-add-keywords nil plain-list-minus)))))

(provide 'mainspring-org-prettify)

;; mainspring-org-prettify.el ends here