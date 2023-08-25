(use-package mainspring-org-prettify
  :defer t
  :straight (:type git :host github :repo "JonathanHope/mainspring-org-prettify" :branch "master" :files ("mainspring-org-prettify.el"))

  :commands (mainspring-org-prettify-mode)

  :init
  (setq mainspring-org-prettify-done ?)
  (setq mainspring-org-prettify-done-pad t)
  (setq mainspring-org-prettify-todo ?)
  (setq mainspring-org-prettify-todo-pad t)
  (setq mainspring-org-prettify-checkbox-unchecked ?)
  (setq mainspring-org-prettify-checkbox-unchecked-pad t)
  (setq mainspring-org-prettify-checkbox-checked ?)
  (setq mainspring-org-prettify-checkbox-checked-pad t)
  (setq mainspring-org-prettify-headline-dash ?━)
  (setq mainspring-org-prettify-headline-bullet ?⬢)
  (setq mainspring-org-prettify-plain-list-plus-char ?➤)
  (setq mainspring-org-prettify-plain-list-asterisk-char ?➤)
  (setq mainspring-org-prettify-plain-list-minus-char ?➤)
  
  (add-hook 'org-mode-hook 'mainspring-org-prettify-mode))

(provide 'init-mainspring-org-prettify)
