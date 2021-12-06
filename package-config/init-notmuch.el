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
          ("tags" . "(%s)")))
  (setq notmuch-fcc-dirs (format-time-string "/home/jhope/Maildir/jhope/Sent/cur/%Y-%m-%d_%T"))
  (setq notmuch-maildir-use-notmuch-insert nil)

  (setq send-mail-function 'sendmail-send-it
        sendmail-program "msmtp"
        mail-specificy-envelope-from t
        message-sendmail-envelope-from 'header
        mail-envelope-from 'header))

(provide 'init-notmuch)
