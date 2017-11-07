;; Package configuration for cc-mode.

(use-package cc-mode
  :defer t

  :mode (("\\.h$" . c++-mode)
         ("\\.cpp$" . c++-mode)
         ("\\.tpp$" . c++-mode))
  :bind
  (:map c++-mode-map
        ("M-o". ff-find-other-file))

  :config
  (defadvice c-lineup-arglist (around my activate)
    "Improve indentation of continued C++11 lambda function opened as argument."
    (setq ad-return-value
          (if (and (equal major-mode 'c++-mode)
                   (ignore-errors
                     (save-excursion
                       (goto-char (c-langelem-pos langelem))
                       ;; Detect "[...](" or "[...]{". preceded by "," or "(",
                       ;;   and with unclosed brace.
                       (looking-at ".*[(,][ \t]*\\[[^]]*\\][ \t]*[({][^}]*$"))))
              0                           ; no additional indent
            ad-do-it)))                   ; default behavior

  :config
  (add-hook 'c-mode-hook (lambda () (c-set-style "k&r"))))

(provide 'init-cc-mode)
