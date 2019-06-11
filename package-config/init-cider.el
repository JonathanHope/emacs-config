;; Package configuration for cider

(use-package cider
	:ensure t
  :defer t
  :bind
  (:map cider-repl-mode-map
        ("M-<up>" . cider-repl-backward-input)
        ("M-<down>" . cider-repl-forward-input))

  :init
  (add-hook 'cider-mode-hook #'eldoc-mode)
  (add-hook 'cider-connected-hook 'my-init-cider-connected-hook)

  :config
  ;; When there's a cider error, show its buffer and switch to it
  (setq cider-show-error-buffer t)
  (setq cider-auto-select-error-buffer t)
  ;; Where to store the cider history.
  (setq cider-repl-history-file "~/.emacs.d/cider-history")
  ;; Disable the REPL welcome message.
  (setq cider-repl-display-help-banner nil)
  ;; Allow the REPL to be used without a project.
  (setq cider-allow-jack-in-without-project t)

  (setq cider-prompt-for-symbol nil)
  (setq nrepl-log-messages nil)

  (require 'nrepl-puget)

  (defun my-init-cider-connected-hook ()
    (let ((src (expand-file-name "~/.init.clj")))
      (if (file-exists-p src)
          (let* ((buf (find-buffer-visiting src))
                 (killp (not buf))
                 (buf (or buf (find-file-noselect src))))
            (unwind-protect
                (cider-load-file src)
              (unless killp
                (kill-buffer buf)))))))

  (setq cider-pprint-fn "zensols.nrpuget.core/pprint"
        cider-repl-use-pretty-printing t))

(provide 'init-cider)