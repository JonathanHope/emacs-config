;; Package configuration for color-identifiers-mode

(use-package color-identifiers-mode
  :ensure t
  :defer t
  :diminish color-identifiers-mode

  :init
  (defun enable-color-identifers-mode ()
    (color-identifiers-mode)
    ;; TODO: Is there a way to have lots of good looking colors? Probably not.
    ;;(setq color-identifiers:colors '("#96b5b4" "#b48ead" "#8fa1b3" "#ebcb8b" "#65737E" "#d08770" "#A6ACB6" "#a3be8c" "#a3c6d0" "#ab7967"))
    (color-identifiers:refresh))

  (add-hook 'clojure-mode-hook 'enable-color-identifers-mode)
  (add-hook 'js-mode-hook 'enable-color-identifers-mode)
  (add-hook 'lua-mode-hook 'enable-color-identifers-mode)
  (add-hook 'c++-mode-hook 'enable-color-identifers-mode)
  (add-hook 'rust-mode-hook 'enable-color-identifers-mode)

  :config

  (setq color-identifiers:timer
        (run-with-idle-timer .5 t 'color-identifiers:refresh))

  (defun my-filter (condp lst)
    (delq nil
          (mapcar (lambda (x) (and (funcall condp x) x)) lst)))

  (setq color-identifiers:modes-alist (my-filter (lambda (entry) (not (equal 'js-mode (nth 0 entry)))) color-identifiers:modes-alist))
  (setq color-identifiers:modes-alist (my-filter (lambda (entry) (not (equal 'c++-mode (nth 0 entry)))) color-identifiers:modes-alist))
  (setq color-identifiers:modes-alist (my-filter (lambda (entry) (not (equal 'c-mode (nth 0 entry)))) color-identifiers:modes-alist))
  (setq color-identifiers:modes-alist (my-filter (lambda (entry) (not (equal 'rust-mode (nth 0 entry)))) color-identifiers:modes-alist))

  (add-to-list
   'color-identifiers:modes-alist
   `(js-mode . (""
                "\\_<\\([a-zA-Z_$]\\(?:\\s_\\|\\sw\\)*\\)"
                (nil font-lock-variable-name-face font-lock-function-name-face))))

  (add-to-list
   'color-identifiers:modes-alist
   `(c++-mode . (""
                 "\\_<\\([a-zA-Z_$]\\(?:\\s_\\|\\sw\\)*\\)"
                 (nil font-lock-variable-name-face font-lock-function-name-face))))

  (add-to-list
   'color-identifiers:modes-alist
   `(rust-mode . (""
                  "\\_<\\([a-zA-Z_$]\\(?:\\s_\\|\\sw\\)*\\)"
                  (nil font-lock-variable-name-face font-lock-function-name-face))))

  (add-to-list
   'color-identifiers:modes-alist
   `(lua-mode . (""
                 "\\_<\\([a-zA-Z_$@]\\(?:\\s_\\|\\sw\\)*\\)"
                 (nil font-lock-variable-name-face prog-mode-font-lock-function-name-face font-lock-type-face))))

  (defun c++-mode-get-declarations ()
    ""
    (let ((result nil))
      (save-excursion
        (goto-char (point-min))
        (catch 'end-of-file
          (while t
            (let ((next-change (next-property-change (point))))
              (if (not next-change)
                  (throw 'end-of-file nil)
                (goto-char next-change)
                (when (or (eq (get-text-property (point) 'face) 'font-lock-variable-name-face)
                          (eq (get-text-property (point) 'face) 'font-lock-function-name-face)
                          (get-text-property (point) 'color-identifiers:fontified))
                  (push (substring-no-properties (symbol-name (symbol-at-point))) result)))))))
      (delete-dups result)
      result))

  (setq color-identifiers:mode-to-scan-fn-alist (my-filter (lambda (entry) (not (equal 'c++-mode (nth 0 entry)))) color-identifiers:mode-to-scan-fn-alist))
  (color-identifiers:set-declaration-scan-fn 'c++mode 'c++-mode-get-declarations))

(provide 'init-color-identifiers-mode)
