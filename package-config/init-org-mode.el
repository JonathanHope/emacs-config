(use-package org
  :defer t
  :straight org-plus-contrib

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
  (setq org-src-window-setup 'current-window)
  (setq org-adapt-indentation nil)
  (setq org-plantuml-jar-path (expand-file-name "~/.emacs.d/jar/plantuml.jar"))
  (setq org-startup-truncated nil)

  (defun set-org-mode-font ()
    (interactive)
    (setq buffer-face-mode-face '(:family "PragmataPro" :height 90))
    (buffer-face-mode))

  (add-hook 'org-mode-hook 'set-org-mode-font)
  (add-hook 'org-mode-hook 'prettify-symbols-mode)

  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((octave . t)
     (clojure . t)
     (plantuml . t)
     (restclient . t)))

  (add-to-list 'org-src-lang-modes '("xml" . sgml))
  (add-to-list 'org-src-lang-modes '("ebnf" . ebnf))
  (add-to-list 'org-src-lang-modes '("json" . javascript))

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
