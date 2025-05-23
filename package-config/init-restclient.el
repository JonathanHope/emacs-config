;; -*- lexical-binding: t; -*-
(use-package restclient
  :defer t
  :straight t

  :mode (("\\.http$" . restclient-mode))

  :bind
  (:map restclient-mode-map
        ("C-<tab>" . mainspring-hydra-restclient/body))

  :init
  (defvar my-restclient-token nil)
  (defun my-restclient-hook ()
    "Update token from a request."
    (save-excursion
      (save-match-data
        ;; update regexp to extract required data
        (when (re-search-forward "\"access_token\":\"\\(.*?\\)\"" nil t)
          (setq my-restclient-token (match-string 1))))))

  (add-hook 'restclient-response-received-hook #'my-restclient-hook))

(provide 'init-restclient)
