(use-package cider
  :straight t
  :defer t

  :bind
  (:map cider-repl-mode-map
        ("M-<up>" . cider-repl-backward-input)
        ("M-<down>" . cider-repl-forward-input)
        ("C-<tab>" . mainspring-hydra-clojure-cider/body))

  :init
  (add-hook 'cider-mode-hook #'eldoc-mode)

  (setq cider-repl-history-file "~/.emacs.d/cider-history")
  (setq cider-repl-display-help-banner nil)
  (setq cider-allow-jack-in-without-project t)
  (setq cider-prompt-for-symbol nil)
  (setq nrepl-log-messages nil)
  (setq nrepl-hide-special-buffers t)
  (setq cider-print-fn 'puget)
  (setq cider-print-options '(("seq-limit" 50)))
  (setq cider-print-quota 51200)
  (setq cider-repl-pop-to-buffer-on-connect nil)
  (setq cider-repl-display-in-current-window nil)
  (setq cider-default-cljs-repl 'shadow)
  (setq cider-clojure-cli-global-options "-R:cider-clj")
  (setq cider-show-error-buffer nil))

(provide 'init-cider)
