(use-package cider
	:ensure t
  :defer t

  :bind
  (:map cider-repl-mode-map
        ("M-<up>" . cider-repl-backward-input)
        ("M-<down>" . cider-repl-forward-input)
        ("C-<tab>" . mainspring-hydra-clojure-cider/body))

  :init
  (add-hook 'cider-mode-hook #'eldoc-mode)
  (add-hook 'cider-connected-hook 'mainspring-init-cider-connected-hook)

  (setq cider-show-error-buffer t)
  (setq cider-auto-select-error-buffer t)
  (setq cider-repl-history-file "~/.emacs.d/cider-history")
  (setq cider-repl-display-help-banner nil)
  (setq cider-allow-jack-in-without-project t)
  (setq cider-prompt-for-symbol nil)
  (setq nrepl-log-messages nil)
  (setq nrepl-hide-special-buffers t)
  (setq cider-pprint-fn "zensols.nrpuget.core/pprint")
  (setq cider-repl-use-pretty-printing t)

  :config
  (require 'nrepl-puget)

  (defun mainspring-init-cider-connected-hook ()
    (let ((src (expand-file-name "~/.init.clj")))
      (if (file-exists-p src)
          (let* ((buf (find-buffer-visiting src))
                 (killp (not buf))
                 (buf (or buf (find-file-noselect src))))
            (unwind-protect
                (cider-load-file src)
              (unless killp
                (kill-buffer buf))))))
    (cider-find-and-clear-repl-output t)))

(provide 'init-cider)
