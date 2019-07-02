(use-package org
  :defer t
  :straight t

  :mode (("\\.org$" . org-mode))

  :bind
  (:map  org-mode-map
         ("C-<tab>" . mainspring-hydra-org/body)
         ("C-S-r" . counsel-org-agenda-headlines)
         ("C-r" . counsel-org-goto))

  :init
  (setq org-startup-folded 0)
  (setq org-ellipsis " …")
  (setq org-src-fontify-natively t)
  (setq org-confirm-babel-evaluate nil)
  (setq org-hide-emphasis-markers t)
  (setq org-src-preserve-indentation t)
  (setq org-src-window-setup 'other-window)
  (setq org-adapt-indentation nil)
  (setq org-plantuml-jar-path (expand-file-name "~/.emacs.d/jar/plantuml.jar"))
  (setq org-startup-truncated nil)

  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((octave . t)
     (clojure . t)
     (plantuml . t)))

  (add-to-list 'org-src-lang-modes '("xml" . sgml))
  (add-to-list 'org-src-lang-modes '("ebnf" . ebnf))
  (add-to-list 'org-src-lang-modes '("json" . javascript))

  (require 'ob-clojure)
  (setq org-babel-clojure-backend 'cider)

  (defun turn-on-org-show-all-inline-images ()
    (org-display-inline-images t t)
    (org-update-statistics-cookies t))

  (add-hook 'org-mode-hook 'turn-on-org-show-all-inline-images)
  (add-hook 'org-babel-after-execute-hook 'org-display-inline-images)

  (add-to-list 'org-file-apps '("\\.xls\\'" . default))
  (add-to-list 'org-file-apps '("\\.xlsx\\'" . default))

  (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.1)))

(provide 'init-org-mode)
