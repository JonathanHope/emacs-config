;; Package configuration for Octave.

(use-package octave-mode
  :defer t

  :mode
  (("\\.m$" . octave-mode))

  :init
  (add-hook 'octave-mode-hook
            (lambda ()
              (define-key octave-mode-map (kbd "C-<tab>") 'octave-hydra/body))))

(provide 'init-octave-mode)
