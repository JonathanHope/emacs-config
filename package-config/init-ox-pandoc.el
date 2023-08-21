(use-package ox-pandoc
  :straight t
  :defer t

  :init
  (setq org-pandoc-options '((wrap . "none"))))

(provide 'init-ox-pandoc)
