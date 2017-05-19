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

;; Non-package related emacs config.
(require 'init-emacs)

;; Setting up the mode-line.
(require 'init-mode-line)

;; Support for projects.
(require 'init-projectile)

;; Setting up top level keybindings
;; I prefer CUA style bindings.
(require 'init-bind-key)

;; Completion.
;; Most things should have fuzzy completion in a minibuffer.
(require 'init-async)
(require 'init-flx)
(require 'init-ivy)
(require 'init-counsel)
(require 'init-counsel-projectile)
(require 'init-expand-region)
(require 'init-company)

;; More intuitive undo and redo behavior.
(require 'init-undo-tree)

;; A more intuitive window management solution.
;; Numbers windows for easy window changing.
;; The currently focused window will always be the largest.
(require 'init-winum)
(require 'init-golden-ratio)
(require 'init-shackle)

;; A way to move around in the current page of the buffer without a mouse.
(require 'init-avy)

;; Simple keyboard driven popup menus.
(require 'init-hydra)

;; Structural editing everywhere.
(require 'init-smartparens)

;; Better integration with system clipboards.
(require 'init-simpleclip)

;; Snippet expansion.
(require 'init-yasnippet)

;; More intuitive regexp replace.
(require 'init-visual-regexp)

;; Generic code folding solution.
(require 'init-yafolding)

;; A way to move lines or selected text around.
(require 'init-move-text)

;; Enforce constant indentation correction.
(require 'init-aggressive-indent)

;; Quckly jump between functions in a buffer.
(require 'init-iedit)

;; Jump to the definition of things.
(require 'init-dumb-jump)

;; Special syntax highlighting.
(require 'init-rainbow-delimiters)
(require 'init-highlight-numbers)
(require 'init-color-identifiers-mode)

;; Modes

;; Show the color of color codes as a background color.
(require 'init-rainbow-mode)

;; Spell checking.
(require 'init-flyspell)
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
