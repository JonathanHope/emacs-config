(use-package notmuch
  :straight t
  :defer t

  :bind
  (:map notmuch-hello-mode-map
   ("C-<tab>" . mainspring-hydra-notmuch-hello/body)
   :map notmuch-search-mode-map
   ("C-<tab>" . mainspring-hydra-notmuch-search/body)
   :map notmuch-show-mode-map
   ("C-<tab>" . mainspring-hydra-notmuch-show/body)
   :map notmuch-message-mode-map
   ("C-<tab>" . mainspring-hydra-notmuch-message/body))

  :init
  (setq notmuch-show-logo nil)
  (setq notmuch-hello-sections
   '(notmuch-hello-insert-header))
  (setq notmuch-search-result-format
  `(("date" . "%12s ")
    ("authors" . "%-20s ")
    ("subject" . "%s ")
    ("tags" . "(%s)"))))

(provide 'init-notmuch)
