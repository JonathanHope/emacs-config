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
(require 'bind-key)

;; Add directory for local packages.
(add-to-list 'load-path "~/.emacs.d/local")

;; Add directory for various customizations.
(add-to-list 'load-path "~/.emacs.d/customizations")

;; Load up constants used throughout emacs.
(load "constants.el")

;; Configure packages using use-package.
(add-to-list 'load-path "~/.emacs.d/packages")
(require 'init-custom-keymap)
(require 'init-undo-tree)
(require 'init-async)
(require 'init-helm)
(require 'init-projectile)
(require 'init-multiple-cursors)
(require 'init-expand-region)
(require 'init-ace-jump-mode)
(require 'init-window-numbering)
(require 'init-hydra)	
(require 'init-company)
(require 'init-rainbow-mode)
(require 'init-isearch)
(require 'init-org-mode)
(require 'init-magit)
(require 'init-paredit)
(require 'init-rainbow-delimiters)
(require 'init-clojure-mode)
(require 'init-cider)
(require 'init-markdown-mode)

;; Load up the custom functions.
(load "functions.el")

;; Configure Emac's behavior.
(load "behavior.el")

;; Configure Emac's interface.
(load "interface.el")

;; Configure the keyboard driven menus.
(load "hydra-menus.el")

;; Run the startup function.
(startup)