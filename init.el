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

;; Prevent custom from polluting my init file.
(setq custom-file "~/.emacs.d/custom.el")

;; Configure packages using use-package.
(add-to-list 'load-path "~/.emacs.d/package-config")

;; Constants
(setq old-default-directory default-directory)
(setq notes-directory "~/Notes/")
(setq scratch-directory "~/Notes/Scratch/")
(setq scratch-files (list "scratch.txt" "scratch.clj" "scratch.m"))
(setq projects-directory "~/Projects/")

;; Org file support.
;; This has to be done first so everything compiles agains the right org version.
(require 'init-org-mode)
(require 'init-mainspring-org-prettify)
(require 'init-org-pretty-table)

;; Core

;; Shouldn't need this.
(require 'subr-x)

;; Non-package related emacs config.
(require 'init-emacs)
(require 'windmove)

;; Setting up the mode-line.
(require 'init-mainspring-mode-line)

;; Support for projects.
(require 'init-projectile)

;; Setting up top level keybindings
;; I prefer CUA style bindings.
(require 'init-bind-key)

;; Completion.
;; Most things should have fuzzy completion in a minibuffer.
(require 'init-flx)
(require 'init-ivy)
(require 'init-counsel)
(require 'init-counsel-projectile)
(require 'init-company)

;; Ability to expand selections based on syntax.
(require 'init-expand-region)

;; More intuitive undo and redo behavior.
(require 'init-undo-tree)

;; A more intuitive window management solution.
;; Numbers windows for easy window changing.
;; The currently focused window will always be the largest.
(require 'init-winum)
(require 'init-shackle)
(require 'init-posframe)

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

;; Enforce constant indentation correction.
(require 'init-aggressive-indent)

;; Quckly jump between functions in a buffer.
(require 'init-iedit)

;; Jump to the definition of things.
(require 'init-dumb-jump)

;; Special syntax highlighting.
(require 'init-rainbow-delimiters)
(require 'init-highlight-numbers)

;; Allow shortcuts to be tied to a region being selected.
(require 'init-selected)

;; Jump to a visible location.
(require 'init-avy)

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

;; Master TOOD list.
(require 'init-slate)

;; Config file formats.

;; XML support.
(require 'init-sgml-mode)

;; JSON support.
(require 'init-js-mode)

;; YAML support.
(require 'init-yaml-mode)

;; Design tools.

;; REST APIs.
(require 'init-restclient)

;; UML/Flows.
(require 'init-plantuml-mode)

;; Programming languages

;; EBNF support.
(require 'init-ebnf-mode)

;; Clojure support.
(require 'init-clojure-mode)
(require 'init-cider)

;; Octave support.
(require 'init-octave-mode)

;; Run the startup function.
(mainspring-startup)
