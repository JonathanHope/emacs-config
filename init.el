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
(require 'init-ox-jira)
(require 'init-ox-confluence)
(require 'init-ob-restclient)
(require 'init-org-mode)
(require 'init-mainspring-org-prettify)
(require 'init-org-contrib)
(require 'init-ox-reveal)
(require 'init-htmlize)
(require 'init-olivetti)
(require 'init-flycheck-languagetool)

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

;; Jump to the definition of things.
(require 'init-dumb-jump)

;; Special syntax highlighting.
(require 'init-rainbow-delimiters)
(require 'init-highlight-numbers)
(require 'init-hl-todo)
(require 'init-treesit-parser-manager)

;; Allow shortcuts to be tied to a region being selected.
(require 'init-selected)

;; Jump to a visible location.
(require 'init-avy)

;; Comint color.
(require 'init-ansi-color)

;; Apps

;; Show the color of color codes as a background color.
(require 'init-rainbow-mode)

;; Spell checking.
(require 'init-flyspell)
(require 'init-flyspell-correct)

;; File browser.
(require 'init-dired)

;; Git support.
(require 'init-magit)

;; Shell.
(require 'init-eshell)

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

;; UML/Flows.
(require 'init-plantuml-mode)

;; DevOps stuff

;; Docker
(require 'init-dockerfile-mode)

;; Smerge
(require 'init-smerge)

;; Markdown
(require 'init-markdown-mode)

;; CMake
(require 'init-cmake-mode)

;; Nix
(require 'init-nix-mode)

;; Fish
(require 'init-fish-mode)

;; Terraform
(require 'init-terraform-mode)

;; GraphQL
(require 'init-graphql-mode)

;; Programming languages

;; EBNF support.
(require 'init-ebnf-mode)

;; Clojure support.
(require 'init-clojure-mode)
(require 'init-cider)

;; Octave support.
(require 'init-octave-mode)

;; Typescript support.
(require 'init-typescript-mode)

;; Run the startup function.
(mainspring-startup)
