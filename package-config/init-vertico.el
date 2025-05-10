(use-package vertico
  :straight (vertico :files (:defaults "extensions/*")
                     :includes (vertico-indexed
                                vertico-flat
                                vertico-grid
                                vertico-mouse
                                vertico-quick
                                vertico-buffer
                                vertico-repeat
                                vertico-reverse
                                vertico-directory
                                vertico-multiform
                                vertico-unobtrusive
                                ))

  :init
  (setq vertico-count-format nil)

  :config
  (vertico-mode)
  (vertico-multiform-mode)

  (add-to-list 'vertico-multiform-categories '(embark-keybinding grid)))

(provide 'init-vertico)
