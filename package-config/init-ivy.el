(use-package ivy
  :ensure t

  :bind
  (:map ivy-minibuffer-map
        ("<tab>" . ivy-alt-done)
        ("S-<return>" . ivy-immediate-done))

  :init
  (setq ivy-extra-directories nil)
  (setq enable-recursive-minibuffers t)
  (setq ivy-height 20)
  (setq ivy-initial-inputs-alist nil)
  (setq ivy-count-format "")
  (setq ivy-re-builders-alist
        '((t . ivy--regex-fuzzy)))

  :config
  (ivy-mode 1))

(provide 'init-ivy)
