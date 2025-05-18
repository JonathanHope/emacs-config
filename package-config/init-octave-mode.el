;; -*- lexical-binding: t; -*-
(use-package octave-mode
  :defer t

  :mode
  (("\\.m$" . octave-mode))

  :init
  (add-hook 'octave-mode-hook
            (lambda ()
              (define-key octave-mode-map (kbd "C-<tab>") 'mainspring-hydra-octave/body)))

  (setq octave-mode-startup-message nil))

(provide 'init-octave-mode)
