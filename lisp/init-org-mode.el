;; Package configuration for org-mode.

(use-package org
  :defer t
  :mode (("\\.org$" . org-mode))

  :bind
  (:map  org-mode-map
         ("C-<tab>" . org-hydra-top/body))

  :config
  ;; Add a timestamp when completing something.
  (setq org-log-done t)

  ;; Don't start the documents up folded.
  (setq org-startup-folded 0)

  ;; Set the folder the notes are kept in.
  (setq org-agenda-files (list notes-directory))

  ;; Change ellipsis to something else.
  (setq org-ellipsis " …")

  (setq org-src-fontify-natively t)

  ;; Substitute a wide variety of characters for prettier characters.
  (add-hook 'org-mode-hook
            (lambda ()
              (push '("1." . ?❶) prettify-symbols-alist)
              (push '("2." . ?❷) prettify-symbols-alist)
              (push '("3." . ?❸) prettify-symbols-alist)
              (push '("4." . ?❹) prettify-symbols-alist)
              (push '("5." . ?❺) prettify-symbols-alist)
              (push '("6." . ?❻) prettify-symbols-alist)
              (push '("7." . ?❼) prettify-symbols-alist)
              (push '("8." . ?❽) prettify-symbols-alist)
              (push '("9." . ?❾) prettify-symbols-alist)
              (push '("10." . ?❿) prettify-symbols-alist)

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
              (push '("******" . (?━ (Br . Bl) ?━ (Br . Bl) ?━ (Br . Bl) ?━ (Br . Bl) ?━ (Br . Bl) ?━ (Br . Bl) ?⬢)) prettify-symbols-alist))))

(provide 'init-org-mode)
