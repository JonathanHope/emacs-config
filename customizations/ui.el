;; Hide menu and toobar.
(menu-bar-mode -1)
(tool-bar-mode -1)

;; Show line numbers
(global-linum-mode)

;; Disable redundant scroll bars.
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

;; Set font
(set-default-font "Envy Code R-10")

;; Disable bell
(setq ring-bell-function 'ignore)

;; Scroll one line at a time.
(setq redisplay-dont-pause t
  scroll-margin 1
  scroll-step 1
  scroll-conservatively 10000
  scroll-preserve-screen-position 1)

;; Set cursor type.
(setq-default cursor-type 'bar)

;; Set color scheme.
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(add-to-list 'load-path "~/.emacs.d/themes")
(load-theme 'base16-ocean-dark t)