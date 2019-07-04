(use-package shackle
  :straight t

  :init
  (setq shackle-rules
        '((compilation-mode :select nil)
          ("*Help*" :select t :same t)
          ("*info*" :select t :popup t :align right)
          (".*Export.*" :regexp t :select t :same t)
          (".*cider-repl.*" :regexp t :select nil :other t :align right :size 0.5)))
  :config
  (shackle-mode 1))

(provide 'init-shackle)
