(use-package shackle
  :straight t

  :init
  (setq shackle-rules
        '((compilation-mode :select nil)
          ("*Help*" :select t :align right)
          ("*info*" :select t :popup t :align right)
          (".*Org Src.*" :regexp t :select t :popup t :align bottom)
          (".*cider-repl.*" :regexp t :select nil :other t :align right :size 0.5)))
  :config
  (shackle-mode 1))

(provide 'init-shackle)
