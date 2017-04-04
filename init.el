;; Set Emacs to use UTF8
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

;; Use Emac's package management.
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
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
(setq scratch-files (list "scratch.sql" "scratch.clj" "scratch.js" "scratch.txt" "scratch.html" "scratch.css" "scratch.bat" "scratch.cs"))
(setq projects-directory "~/Projects/")

;; Core
(require 'init-projectile)
(require 'init-emacs)
(require 'init-mode-line)
(require 'init-bind-key)
(require 'init-undo-tree)
(require 'init-async)
(require 'init-flx)
(require 'init-ivy)
(require 'init-counsel)
(require 'init-counsel-projectile)
(require 'init-expand-region)
(require 'init-avy)
(require 'init-winum)
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
(require 'init-move-text)
(require 'init-flyspell)
(require 'init-aggressive-indent)
(require 'init-highlight-numbers)
(require 'init-shackle)
(require 'init-iedit)
(require 'init-dumb-jump)
(require 'init-flyspell-correct)

;; Org file support.
(require 'init-org-mode)

;; File browser.
(require 'init-dired)

;; Git support.
(require 'init-magit)

;; Shell
(require 'init-eshell)

;; Clojure support.
(require 'init-clojure-mode)
(require 'init-cider)

;; Markdown support.
(require 'init-markdown-mode)
(require 'init-livedown)

;; XML support.
(require 'init-sgml-mode)

;; HTML support.
(require 'init-web-mode)
(require 'init-impatient-mode)

;; Javascript support.
(require 'init-js-mode)
(require 'init-nodejs-repl)

;; CSS support.
(require 'init-css-mode)

;; Bat support.
(require 'init-bat-mode)

;; C# support.
(require 'init-csharp-mode)

;; EBNF support.
(require 'init-ebnf-mode)

;; Restclient support.
(require 'init-restclient)

;; Run the startup function.
(startup)
