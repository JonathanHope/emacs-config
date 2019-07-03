(use-package mainspring-org-prettify
  :defer t
  :straight (:type git :host github :repo "JonathanHope/mainspring-org-prettify" :branch "master" :files ("mainspring-org-prettify.el"))

  :commands (mainspring-org-prettify-mode)

  :init
  (add-hook 'org-mode-hook 'mainspring-org-prettify-mode))

(provide 'init-mainspring-org-prettify)
