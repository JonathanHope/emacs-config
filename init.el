;; Set Emacs to use UTF8
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

;; Enable straight
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Integrate straight with use-package.
(straight-use-package 'use-package)

;; Configure packages using use-package.
(add-to-list 'load-path "~/.emacs.d/package-config")

;; Prevent custom from polluting my init file.
(setq custom-file "~/.emacs.d/custom.el")

;; Constants
(setq old-default-directory default-directory)
(setq notes-directory "~/Notes/")
(setq scratch-directory "~/Notes/Scratch/")
(setq scratch-files (list "scratch.txt" "scratch.clj" "scratch.m" "scratch.org"))
(setq projects-directory "~/Projects/")

;; Org file support.
;; This has to be done first so everything compiles agains the right org version.
(require 'init-org-mode)
(require 'init-org-contrib)
(require 'init-ob-restclient)
(require 'init-ob-typescript)
(require 'init-mainspring-org-prettify)
(require 'init-htmlize)
(require 'init-ox-reveal)
(require 'init-ox-pandoc)
(require 'init-olivetti)
(require 'init-jinx)

;; Core

;; Setting up the mode-line.
(require 'init-mainspring-mode-line)

;; Non-package related emacs config.
(require 'init-emacs)

;; Support for projects.
(require 'init-projectile)

;; Setting up top level keybindings
;; I prefer CUA style bindings.
(require 'init-bind-key)

;; Completion.
;; Most things should have fuzzy completion in a minibuffer.
(require 'init-vertico)
(require 'init-orderless)
(require 'init-marginalia)
(require 'init-consult)
(require 'init-consult-projectile)
(require 'init-embark)
(require 'init-embark-consult)
(require 'init-corfu)
(require 'init-consult-todo)

;; Ability to expand selections based on syntax.
(require 'init-expand-region)

;; More intuitive undo and redo behavior.
(require 'init-undo-fu)

;; A more intuitive window management solution.
;; Numbers windows for easy window changing.
(require 'init-winum)
(require 'init-shackle)
(require 'init-posframe)
(require 'windmove)

;; Simple keyboard driven popup menus.
(require 'init-hydra)

;; Structural editing everywhere.
(require 'init-smartparens)

;; Better integration with system clipboards.
(require 'init-simpleclip)

;; More intuitive regexp replace.
(require 'init-pcre2el)
(require 'init-visual-regexp)

;; A way to move lines or selected text around.
(require 'init-move-text)

;; Quckly jump between functions in a buffer.
(require 'init-iedit)

;; Special syntax highlighting.
(require 'init-rainbow-delimiters)
(require 'init-highlight-numbers)
(require 'init-hl-todo)

;; Allow shortcuts to be tied to a region being selected.
(require 'init-selected)

;; Jump to a visible location.
(require 'init-avy)

;; Comint color.
(require 'init-ansi-color)

;; Auto format code.
(require 'init-apheleia)

;; Apps

;; Show the color of color codes as a background color.
(require 'init-rainbow-mode)

;; File browser.
(require 'init-dired)
(require 'init-dired-subtree)
(require 'init-nerd-icons-dired)

;; Git support.
(require 'init-magit)

;; Shell.
(require 'init-eshell)
(require 'init-eat)

;; Calculator.
(require 'init-calc)

;; Notes manager.
(require 'init-deft)

;; Master TODO list.
(require 'init-slate)

;; Buffer manager.
(require 'init-ibuffer)

;; Email
(require 'init-notmuch)

;; Config file formats.

;; XML support.
(require 'init-sgml-mode)

;; JSON support (tree-sitter).
(require 'init-json-mode)

;; YAML support (tree-sitter).
(require 'init-yaml-mode)

;; Design tools.

;; REST APIs.
(require 'init-restclient)

;; User stories
(require 'init-feature-mode)

;; Diagrams (tree-sitter)
(require 'init-mermaid-mode)
(require 'init-ob-mermaid)

;; DevOps stuff

;; Docker (tree-sitter)
(require 'init-dockerfile-mode)

;; Smerge
(require 'init-smerge)

;; Markdown
(require 'init-markdown-mode)

;; CMake (tree-sitter)
(require 'init-cmake-mode)

;; Nix
(require 'init-nix-mode)

;; Fish
(require 'init-fish-mode)

;; Terraform
(require 'init-terraform-mode)

;; GraphQL
(require 'init-graphql-mode)

;; Bash (tree-sitter)
(require 'init-bash-mode)

;; TOML (tree-sitter)
(require 'init-toml-mode)

;; Programming languages

;; EBNF support.
(require 'init-ebnf-mode)

;; Clojure support.
(require 'init-clojure-mode)
(require 'init-cider)

;; Octave support.
(require 'init-octave-mode)

;; Typescript support (tree-sitter).
(require 'init-typescript-mode)
(require 'init-jsdoc)

;; CSS support (tree-sitter).
(require 'init-css-mode)

;; HTML support (tree-sitter).
(require 'init-html-mode)

;; Go support (tree-sitter).
(require 'init-go-mode)

;; SQL support.
(require 'init-sql-indent)

;; Run the startup function.
(mainspring-startup)
