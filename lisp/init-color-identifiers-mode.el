;; Package configuration for color-identifiers-mode

(use-package color-identifiers-mode
  :ensure t
  :defer t
  :diminish color-identifiers-mode

  :init
  (defun enable-color-identifers-mode ()
    (color-identifiers-mode)
    (setq color-identifiers:colors '("#96b5b4" "#b48ead" "#8fa1b3" "#ebcb8b" "#65737E" "#d08770" "#A6ACB6" "#a3be8c" "#a3c6d0" "#ab7967"))
    (color-identifiers:refresh))

  (add-hook 'clojure-mode-hook 'enable-color-identifers-mode)
  (add-hook 'js-mode-hook 'enable-color-identifers-mode)

  :config

  (setq color-identifiers:timer
        (run-with-idle-timer .5 t 'color-identifiers:refresh))

  (defun my-filter (condp lst)
    (delq nil
          (mapcar (lambda (x) (and (funcall condp x) x)) lst)))

  (setq color-identifiers:modes-alist (my-filter (lambda (entry) (not (equal 'js-mode (nth 0 entry)))) color-identifiers:modes-alist))
  (add-to-list
   'color-identifiers:modes-alist
   `(js-mode . (""
                "\\_<\\([a-zA-Z_$]\\(?:\\s_\\|\\sw\\)*\\)"
                (nil prog-mode-font-lock-variable-name-face prog-mode-font-lock-function-name-face)))))

(provide 'init-color-identifiers-mode)
