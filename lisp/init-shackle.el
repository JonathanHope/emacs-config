;; Package configuration for shackle

(use-package shackle
  :ensure t
  :diminish shackle-mode

  :init
  (setq helm-display-function 'pop-to-buffer)
  (setq shackle-rules
        '((compilation-mode :select nil)
          ("[Hh]elm" :regexp t :align bottom :size 0.333)
          ("*Help*" :select t :align right)))
  :config
  ;; A bit of trickery to have the finaly layout be adjusted by golden ratio.
  (defun adjust-ratio-golden-ratio (buffer alist plist)
    (progn
      (golden-ratio)))

  (add-function :after (symbol-function 'shackle-display-buffer) #'adjust-ratio-golden-ratio)

  (shackle-mode 1))

(provide 'init-shackle)
