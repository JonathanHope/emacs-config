(use-package selected
  :ensure t
  :defer 0
  :commands selected-minor-mode
  :bind (:map selected-keymap
              ("<tab>" . mainspring-indent-region)
              ("<backtab>" . mainspring-unindent-region))

  :init
  (selected-minor-mode)

  :config
  (defun mainspring-indent-region (N)
    (interactive "p")
    (if (use-region-p)
        (progn (indent-rigidly (region-beginning) (region-end) (* N 2))
               (setq deactivate-mark nil))
      (self-insert-command N)))

  (defun mainspring-unindent-region (N)
    (interactive "p")
    (if (use-region-p)
        (progn (indent-rigidly (region-beginning) (region-end) (* N -2))
               (setq deactivate-mark nil))
      (self-insert-command N))))

(provide 'init-selected)
