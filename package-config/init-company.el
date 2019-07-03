(use-package company
  :straight t
  :defer t

  :bind
  (:map company-active-map
        ("<escape>" . company-abort)
        ("<tab>" . company-complete-selection))
  :init
  (add-hook 'cider-repl-mode-hook #'company-mode)
  (add-hook 'cider-mode-hook #'company-mode)

  (setq company-tooltip-limit 10)
  (setq company-tooltip-offset-display 'lines)
  (setq company-dabbrev-downcase 0)
  (setq company-idle-delay 0)
  (setq company-echo-delay 0)
  (setq company-minimum-prefix-length 2)
  (setq company-selection-wrap-around t)
  (setq company-tooltip-align-annotations t)
  (setq company-require-match nil)
  (setq company-transformers '(company-sort-by-occurrence)))

(provide 'init-company)
