(use-package org-pretty-table
  :defer t
  :straight (:type git :host github :repo "Fuco1/org-pretty-table" :branch "master")

  :commands (turn-on-org-pretty-table-mode)

  :init
  (add-hook 'org-mode-hook (lambda () (turn-on-org-pretty-table-mode))))

(provide 'init-org-pretty-table)
