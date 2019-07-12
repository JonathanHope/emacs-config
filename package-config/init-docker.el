(use-package docker
  :defer t
  :straight t

  :init
  (setq docker-image-run-arguments '("-i" "--rm"))

  :config
  (use-package tablist
    :bind
    (:map tablist-minor-mode-map
          ("C-<tab>" . mainspring-hydra-docker/body))))

(provide 'init-docker)
