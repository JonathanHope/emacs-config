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

;; Update packages.
(when (not package-archive-contents)
  (package-refresh-contents))

;; List of packages to install.
(defvar my-packages
'(
  undo-tree
  projectile
  ido-ubiquitous
  smex
  multiple-cursors))

;; Install packages.
(dolist (p my-packages)
  (when (not (package-installed-p p))
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

;; Custom keyboard shortcuts.
(load "keys.el")