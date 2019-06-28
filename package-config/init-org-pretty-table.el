(use-package org-pretty-table
  :defer t
  :commands (turn-on-org-pretty-table-mode)

  :init
  (add-hook 'org-mode-hook (lambda () (turn-on-org-pretty-table-mode))))

(provide 'init-org-pretty-table)
