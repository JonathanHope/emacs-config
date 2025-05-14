(use-package consult-notes
  :straight (:type git :host github :repo "mclear-tools/consult-notes")
  :commands (consult-notes
             consult-notes-search-in-all-notes)
  :config
  (defun mainspring-denote-annotate (cand))
  (setq consult-notes-denote-annotate-function #'mainspring-denote-annotate)
  
  (consult-notes-denote-mode)

  (consult-customize consult-notes :preview-key nil)

  (setq consult-notes-denote-dir nil))

(provide 'init-consult-notes)
