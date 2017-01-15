;; Set Emacs to use UTF8
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

;; Use Emac's package management.
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

;; Prevent custom from polluting my init file.
(setq custom-file "~/.emacs.d/custom.el")

;; Install just use package.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Enable use package.
(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

;; Add directory for local packages.
(add-to-list 'load-path "~/.emacs.d/site-lisp")

;; Configure packages using use-package.
(add-to-list 'load-path "~/.emacs.d/lisp")

;; Constants
(setq old-default-directory default-directory)
(setq notes-directory "~/Notes/")
(setq scratch-directory "~/Notes/Scratch/")
(setq scratch-files (list "scratch.sql" "scratch.clj" "scratch.js" "scratch.txt"))
(setq projects-directory "~/Projects/")

;; Core
(require 'init-emacs)
(require 'init-custom-keymap)
(require 'init-undo-tree)
(require 'init-async)
(require 'init-helm)
(require 'init-helm-swoop)
(require 'init-projectile)
(require 'init-helm-projectile)
(require 'init-multiple-cursors)
(require 'init-expand-region)
(require 'init-ace-jump-mode)
(require 'init-window-numbering)
(require 'init-rainbow-mode)
(require 'init-hydra) 
(require 'init-company)
(require 'init-rainbow-delimiters)
(require 'init-smartparens)
(require 'init-simpleclip)
(require 'init-yasnippet)
(require 'init-golden-ratio)
(require 'init-visual-regexp)
(require 'init-yafolding)

;; Languages
(require 'init-org-mode)
(require 'init-magit)
(require 'init-clojure-mode)
(require 'init-cider)
(require 'init-markdown-mode)
(require 'init-livedown)
(require 'init-sql)
(require 'init-js-mode)
(require 'init-nodejs-repl)
(require 'init-company-tern)
(require 'init-nxml-mode)
(require 'init-dired)
(require 'init-eshell)

;; Run the startup function.
(startup)
