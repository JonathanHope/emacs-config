;; Package configuration for shackle

(use-package shackle
  :ensure t
  :diminish shackle-mode

  :init
  (setq helm-display-function 'pop-to-buffer)
  (setq shackle-rules
        '((compilation-mode :select nil)
          ("[Hh]elm" :regexp t :align bottom :size 0.333)
          ;;("*Help*" :select t :align right)
          ))

  :config
  (shackle-mode 1))

(provide 'init-shackle)
