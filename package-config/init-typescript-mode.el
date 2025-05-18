;; -*- lexical-binding: t; -*-
(use-package typescript-ts-mode
  :defer t

  :bind
  (:map typescript-ts-mode-map
        ("C-<tab>" . mainspring-hydra-typescript/body))
  (:map tsx-ts-mode-map
        ("C-<tab>" . mainspring-hydra-typescript/body))
  (:map js-ts-mode-map
        ("C-<tab>" . mainspring-hydra-typescript/body))
  

  :mode (("\\.ts$" . typescript-ts-mode)
         ("\\.tsx$" . tsx-ts-mode)
         ("\\.js$" . js-ts-mode)
         ("\\.cjs$" . js-ts-mode)
         ("\\.mjs$" . js-ts-mode)))

(provide 'init-typescript-mode)
