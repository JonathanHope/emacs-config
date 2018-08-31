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
  (package-install 'use-package)
  (package-install 'diminish))

;; Enable use package.
(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

;; Add directory for local packages.
(add-to-list 'load-path "~/.emacs.d/packages")

;; Configure packages using use-package.
(add-to-list 'load-path "~/.emacs.d/package-config")

;; Constants
(setq old-default-directory default-directory)
(setq notes-directory "~/Notes/")
(setq scratch-directory "~/Notes/Scratch/")
(setq scratch-files (list "scratch.txt"))
(setq projects-directory "~/Projects/")

;; Core

;; Non-package related emacs config.
(require 'init-emacs)

;; Setting up the mode-line.
(require 'init-mainspring-mode-line)

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
(require 'init-ivy-xref)
(require 'init-counsel)
(require 'init-counsel-projectile)
;; (require 'init-company)

;; Ability to expand selections based on syntax.
(require 'init-expand-region)

;; More intuitive undo and redo behavior.
(require 'init-undo-tree)

;; A more intuitive window management solution.
;; Numbers windows for easy window changing.
;; The currently focused window will always be the largest.
(require 'init-winum)
(require 'init-golden-ratio)
(require 'init-shackle)

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

;; Apps

;; Show the color of color codes as a background color.
(require 'init-rainbow-mode)

;; Spell checking.
(require 'init-flyspell)
(require 'init-flyspell-correct)

;; Org file support.
(require 'init-org-mode)
(require 'init-mainspring-org-prettify)
(require 'init-org-pretty-table)
(require 'init-restclient)
(require 'init-graphviz-dot-mode)

;; File browser.
(require 'init-dired)

;; Git support.
(require 'init-magit)

;; Shell
(require 'init-eshell)

;; Config file formats.

;; XML support.
(require 'init-sgml-mode)

;; Javascript support.
(require 'init-js-mode)

;; YAML support.
(require 'init-yaml-mode)

;; Programming languages

;; C++ Support
(require 'init-doxygen)
(require 'init-cc-mode)
(require 'init-cmake-mode)

;; Rust Support
(require 'init-rust-mode)

;; EBNF support.
(require 'init-ebnf-mode)

;; Clojure support.
(require 'init-clojure-mode)
(require 'init-cider)

;; Run the startup function.
(startup)
