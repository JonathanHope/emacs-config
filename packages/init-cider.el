(use-package cider
	:ensure t
  :commands (cider cider-connect cider-jack-in)

  :bind 
  (:map cider-repl-mode-map
    ("M-<up>" . cider-repl-backward-input)
    ("M-<down>" . cider-repl-forward-input))

  :config
  ;; When there's a cider error, show its buffer and switch to it
  (setq cider-show-error-buffer t)
  (setq cider-auto-select-error-buffer t)

  ;; Where to store the cider history.
  (setq cider-repl-history-file "~/.emacs.d/cider-history")

  ;; Disable the REPL welcome message.
  (setq cider-repl-display-help-banner nil)

  ;; enable paredit in cider.
  (add-hook 'cider-repl-mode-hook 'paredit-mode)

  ;; Company mode autocomplete.
  (add-hook 'cider-repl-mode-hook 'company-mode)
  (add-hook 'cider-mode-hook 'company-mode))

(provide 'init-cider)