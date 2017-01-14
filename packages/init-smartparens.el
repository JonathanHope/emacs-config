;; Package configuration for smartparens.

(use-package smartparens
  :ensure t
  :defer t
  
  :bind
  (:map smartparens-mode-map
        ("C-S-<right>" . sp-forward-sexp)
        ("C-S-<left>" . sp-backward-sexp)
        ("C-S-k" . sp-kill-sexp)
        ("<backspace>" . sp-kill-region-or-backward-delete))

  :init
  (add-hook 'js-mode-hook #'smartparens-mode)
  (add-hook 'clojure-mode-hook #'smartparens-mode)
  (add-hook 'emacs-lisp-mode-hook #'smartparens-mode)
  (add-hook 'cider-repl-mode-hook #'smartparens-mode)
  (add-hook 'nxml-mode-hook #'smartparens-mode)
  
  :config
  (defadvice js3-enter-key (after fix-sp-state activate)
    (setq sp-last-operation 'sp-self-insert))
  
  (setq
   smartparens-strict-mode t
   sp-autoinsert-if-followed-by-word t
   sp-autoskip-closing-pair 'always
   sp-base-key-bindings 'paredit
   sp-hybrid-kill-entire-symbol nil)

  (sp-use-paredit-bindings)

  (sp-with-modes sp-lisp-modes
    (sp-local-pair "'" nil :actions nil))

  (setq sp-navigate-consider-sgml-tags '(nxml-mode))

  (sp-with-modes '(html-mode sgml-mode nxml-mode)
    (sp-local-pair "<" ">"))

  (sp-pair "*" "*" :wrap "C-S-8")
  
  (defun sp-kill-region-or-backward-delete ()
    (interactive)
    (if (region-active-p)
        (kill-region (region-beginning) (region-end))
      (sp-backward-delete-char))))

(provide 'init-smartparens)
