;; Use emacs package management.
(require 'package)

;; Repositories for packages.
(add-to-list 'package-archives
  '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
  '("tromey" . "http://tromey.com/elpa/") t)
(add-to-list 'package-archives
  '("melpa" . "http://melpa.milkbox.net/packages/") t)

(package-initialize)

;; List of packages to install.
(defvar my-packages
'(
  ;; Core.
  undo-tree
  projectile
  helm
  multiple-cursors
  magit
  expand-region
  ace-jump-mode
  window-numbering

  ;; Clojure.
  clojure-mode
  clojure-mode-extra-font-locking
  paredit
  rainbow-delimiters
  cider
  company

  ;; Markdown
  markdown-mode

  ;; YAML
  yaml-mode

  ;; Docker
  dockerfile-mode))

; Install packages.
(dolist (p my-packages)
  (when (not (package-installed-p p))
  (package-refresh-contents)
  (package-install p)))

;; Directories of elisp configurations.
(add-to-list 'load-path "~/.emacs.d/customizations")

;; Customizations of the look and feel of emacs.
(load "ui.el")

;; Change how emacs edits text.
(load "editing.el")

;; Change how we get around in emacs.
(load "navigation.el")

;; Customizations that don't fit anywhere else.
(load "misc.el")

;; Support for clojure.
(load "clojure.el")

;; Support for yaml.
(load "yaml.el")

;; Support for docker.
(load "docker.el")

;; Custom keyboard shortcuts.
(load "keys.el")
