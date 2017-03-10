;; Package configuration for dired.

(use-package golden-ratio
  :ensure t
  :diminish golden-ratio-mode

  :config
  (golden-ratio-mode 1)
  (setq golden-ratio-exclude-buffer-names '("*Helm Swoop*"))
  (setq golden-ratio-exclude-modes '("magit-mode" "magit-popup-mode"))
  (setq golden-ratio-extra-commands
        (append golden-ratio-extra-commands
                '(winum-select-window-0
                  winum-select-window-1
                  winum-select-window-2
                  winum-select-window-3
                  winum-select-window-4
                  winum-select-window-5
                  winum-select-window-6
                  winum-select-window-7
                  winum-select-window-8
                  winum-select-window-9))))

(provide 'init-golden-ratio)
