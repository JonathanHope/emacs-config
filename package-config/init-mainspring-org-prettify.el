(use-package mainspring-org-prettify
  :defer t
  :commands (mainspring-org-prettify-mode)

  :init
  (add-hook 'org-mode-hook 'mainspring-org-prettify-mode))

(provide 'init-mainspring-org-prettify)
