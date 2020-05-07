(use-package shackle
  :straight t

  :init
  (setq shackle-rules
        '((compilation-mode :same t :inhibit-window-quit t)
          ("*Help*" :select t :same t :inhibit-window-quit t)
          ("*info*" :select t :popup t :align right)
          (".*Export.*" :regexp t :select t :same t :inhibit-window-quit t)
          (".*cider-repl.*" :regexp t :select nil :other t :align right :size 0.5)
          (".*cider-error.*" :regexp t :select nil :same t :inhibit-window-quit t)
          (".*cider-result.*" :regexp t :select nil :same t :inhibit-window-quit t)
          ("*docker-images*" :select nil :same t :inhibit-window-quit t)
          ("*docker-containers*" :select nil :same t :inhibit-window-quit t)
          ("*docker-networks*" :select nil :same t :inhibit-window-quit t)
          ("*docker-volumes*" :select nil :same t :inhibit-window-quit t)
          ("*docker-machines*" :select nil :same t :inhibit-window-quit t)
          ("*run .*" :regexp t :select nil :same t :inhibit-window-quit t)
          ("*Org-Babel Error Output*" :select nil :same t :inhibit-window-quit t)
          ("*mix test*" :select t :same t :inhibit-window-quit t)
          ("*mix execute*" :select t :same t :inhibit-window-quit t)
          ("*HTTP Response*" :select t :same t :inhibit-window-quit t)))
  :config
  (shackle-mode 1))

(provide 'init-shackle)
