(use-package shackle
  :ensure t

  :init
  (setq shackle-rules
        '((compilation-mode :select nil)
          ("*Help*" :select t :align right)
          ("*info*" :select t :popup t :align right)
          (".*Org Src.*" :regexp t :select t :popup t :align bottom)
          (".*cider-repl.*" :regexp t :select t :popup t :align right)))
  :config
  (shackle-mode 1))

(provide 'init-shackle)
