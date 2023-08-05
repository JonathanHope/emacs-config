(use-package org
  :straight t
  :defer t

  :mode (("\\.org$" . org-mode))

  :bind
  (:map  org-mode-map
         ("C-<tab>" . mainspring-hydra-org/body)
         ("C-r" . consult-org-heading))

  :init
  (setq org-startup-folded 0)
  (setq org-ellipsis " …")
  (setq org-src-fontify-natively t)
  (setq org-confirm-babel-evaluate nil)
  (setq org-src-preserve-indentation t)
  (setq org-src-window-setup 'current-window)
  (setq org-adapt-indentation nil)
  (setq org-plantuml-jar-path (expand-file-name "~/.emacs.d/jar/plantuml.jar"))
  (setq org-startup-truncated nil)
  (setq org-element-cache-persistent nil)

  (defun set-org-mode-font ()
    (interactive)
    (setq buffer-face-mode-face '(:family "PragmataPro" :height 90))
    (buffer-face-mode))

  (add-hook 'org-mode-hook 'set-org-mode-font)
  (add-hook 'org-mode-hook 'prettify-symbols-mode)
  (add-hook 'org-mode-hook 'visual-line-mode)

  :config
  (require 'ob-js)
  
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((octave . t)
     (clojure . t)
     (plantuml . t)
     (restclient . t)))

  (add-to-list 'org-src-lang-modes '("xml" . sgml))
  (add-to-list 'org-src-lang-modes '("ebnf" . ebnf))
  (add-to-list 'org-src-lang-modes '("json" . json-ts))
  (add-to-list 'org-src-lang-modes '("yaml" . yaml-ts))
  (add-to-list 'org-src-lang-modes '("dockerfile" . dockerfile-ts))
  (add-to-list 'org-src-lang-modes '("typescript" . typescript-ts))
  (add-to-list 'org-src-lang-modes '("tsx" . tsx-ts))
  (add-to-list 'org-src-lang-modes '("html" . html-ts))
  (add-to-list 'org-src-lang-modes '("css" . css-ts))

  (setq org-babel-clojure-backend 'cider)

  (add-to-list 'org-babel-tangle-lang-exts '("js" . "js"))
  
  (defun turn-on-org-show-all-inline-images ()
    (org-display-inline-images t t)
    (org-update-statistics-cookies t))

  (add-hook 'org-mode-hook 'turn-on-org-show-all-inline-images)
  (add-hook 'org-babel-after-execute-hook 'org-display-inline-images)

  (add-to-list 'org-file-apps '("\\.xls\\'" . default))
  (add-to-list 'org-file-apps '("\\.xlsx\\'" . default))

  (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.1))
  (setq org-taskjuggler-default-reports '("textreport report \"Plan\" {
  formats html
  header '== %title =='
  center -8<-
    <[report id=\"plan\"]>
  ->8-
}
# A traditional Gantt chart with a project overview.
taskreport plan \"\" {
  headline \"Project Plan\"
  columns bsi, name, start, end, note, chart
  loadunit shortauto
  hideresource 1
}")))

(provide 'init-org-mode)
