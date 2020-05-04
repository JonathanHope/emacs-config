(use-package perl-mode
  :defer t

  :mode (("\\.pl$" . perl-mode))

  :config
  (setq perl-indent-level 2)
  (setq perl-continued-statement-offset 2)
  (setq perl-continued-brace-offset -2)
  (setq perl-brace-offset 0)
  (setq perl-brace-imaginary-offset 0)
  (setq perl-label-offset  -2))

(provide 'init-perl-mode)
