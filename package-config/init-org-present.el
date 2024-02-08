(use-package org-present
  :straight t
  :defer t

  :init
  (add-hook 'org-present-mode-hook
               (lambda ()
                 (hide-mode-line-mode)
                 (setq buffer-read-only t)))
  (add-hook 'org-present-mode-quit-hook
               (lambda ()
                 (hide-mode-line-mode)
                 (setq buffer-read-only nil))))

(provide 'init-org-present)
