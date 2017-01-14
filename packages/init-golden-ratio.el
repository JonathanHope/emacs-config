;; Package configuration for dired.

(use-package golden-ratio
  :ensure t
  :config
  (golden-ratio-mode 1)
  (setq golden-ratio-extra-commands
      (append golden-ratio-extra-commands
              '(select-window-0
                select-window-1
                select-window-2
                select-window-3
                select-window-4
                select-window-5
                select-window-6
                select-window-7
                select-window-8
                select-window-9))))

(provide 'init-golden-ratio)
