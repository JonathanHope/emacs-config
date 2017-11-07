;; Package configuration for org-mode.

(use-package org
  :ensure org-plus-contrib
  :defer t

  :mode (("\\.org$" . org-mode))

  :bind
  (:map  org-mode-map
         ("C-<tab>" . org-hydra-top/body))

  :config
  ;; Don't start the documents up folded.
  (setq org-startup-folded 0)

  ;; Set the folder the notes are kept in.
  (require 'f)
  (require 's)
  (setq org-agenda-files (setq org-agenda-files
                               (f-entries "~/Notes/"
                                          (lambda (filename)
                                            (s-ends-with-p ".org" filename))
                                          t)))

  ;; Change ellipsis to something else.
  (setq org-ellipsis " …")

  ;; Source code highlighting in source blocks.
  (setq org-src-fontify-natively t
        org-confirm-babel-evaluate nil)

  ;; Allow fontification through markup characters.
  (setq org-hide-emphasis-markers t)

  ;; Don't change indentation of source code. Just bring it over as is.
  (setq org-src-preserve-indentation t)

  ;; Supported org babel languages.
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((clojure . t)
     (restclient . t)
     (dot . t)))

  ;; Configure clojure babel support.
  (require 'ob-clojure)
  (setq org-babel-clojure-backend 'cider)

  (add-to-list 'org-src-lang-modes '("dot" . graphviz-dot))

  ;; Configure restclient babel support.
  (require 'ob-restclient)

  ;; Enable inline images for org.
  (defun turn-on-org-show-all-inline-images ()
    (org-display-inline-images t t))
  (add-hook 'org-mode-hook 'turn-on-org-show-all-inline-images)
  (add-hook 'org-babel-after-execute-hook 'org-display-inline-images)

  (setq org-startup-truncated nil)

  ;; Substitute a wide variety of characters for prettier characters.
  (add-hook 'org-mode-hook
            (lambda ()
              (push '("1)" . ?❶) prettify-symbols-alist)
              (push '("2)" . ?❷) prettify-symbols-alist)
              (push '("3)" . ?❸) prettify-symbols-alist)
              (push '("4)" . ?❹) prettify-symbols-alist)
              (push '("5)" . ?❺) prettify-symbols-alist)
              (push '("6)" . ?❻) prettify-symbols-alist)
              (push '("7)" . ?❼) prettify-symbols-alist)
              (push '("8)" . ?❽) prettify-symbols-alist)
              (push '("9)" . ?❾) prettify-symbols-alist)
              (push '("10)" . ?❿) prettify-symbols-alist)

              (push '("-" . ?➖) prettify-symbols-alist)
              (push '("+" . ?➕) prettify-symbols-alist)

              (push '("[ ]" . ?⚪) prettify-symbols-alist)
              (push '("[X]" . ?⚫) prettify-symbols-alist)

              (push '("TODO" . ?⬜) prettify-symbols-alist)
              (push '("DONE" . ?⬛) prettify-symbols-alist)

              (push '("->" . ?➔) prettify-symbols-alist)

              (push '("*" . (?━ (Br . Bl) ?⬢)) prettify-symbols-alist)
              (push '("**" . (?━ (Br . Bl) ?━ (Br . Bl) ?⬢)) prettify-symbols-alist)
              (push '("***" . (?━ (Br . Bl) ?━ (Br . Bl) ?━ (Br . Bl) ?⬢)) prettify-symbols-alist)
              (push '("****" . (?━ (Br . Bl) ?━ (Br . Bl) ?━ (Br . Bl) ?━ (Br . Bl) ?⬢)) prettify-symbols-alist)
              (push '("*****" . (?━ (Br . Bl) ?━ (Br . Bl) ?━ (Br . Bl) ?━ (Br . Bl) ?━ (Br . Bl) ?⬢)) prettify-symbols-alist)
              (push '("******" . (?━ (Br . Bl) ?━ (Br . Bl) ?━ (Br . Bl) ?━ (Br . Bl) ?━ (Br . Bl) ?━ (Br . Bl) ?⬢)) prettify-symbols-alist)))

  ;; Support for confluence exporting.
  (require 'cl)
  (require 'ox-confluence))

(provide 'init-org-mode)
