;; Package configuration for shackle

(use-package shackle
  :ensure t

  :init
  (setq shackle-rules
        '((compilation-mode :select nil)
          ("*Help*" :select t :align right)))
  :config
  (shackle-mode 1))

(provide 'init-shackle)
