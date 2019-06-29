(use-package calc
  :defer t

  :bind
  (:map calc-mode-map
        ("C-<tab>" . mainspring-hydra-calc/body))

  :init
  (setq calc-show-banner nil)
  (setq calc-line-numbering nil)

  :config
  (calc-trail-display 0)

  (use-package calc-help
    :commands (calc-help-index-entries calc-describe-function calc-describe-variable calc-describe-key)))

(provide 'init-calc)
