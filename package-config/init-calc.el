(use-package calc
  :defer t

  :bind
  (:map calc-mode-map
        ("C-<tab>" . calc-hydra/body))

  :config
  (setq calc-show-banner nil)
  (setq calc-line-numbering nil)
  (calc-trail-display 0)

  (use-package calc-help
    :commands (calc-help-index-entries calc-describe-function calc-describe-variable calc-describe-key)))

(provide 'init-calc)
