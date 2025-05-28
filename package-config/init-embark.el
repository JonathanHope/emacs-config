;; -*- lexical-binding: t; -*-
(use-package embark
  :straight t

  :init
   (setq embark-prompter #'embark-completing-read-prompter)
   (setq embark-indicators '(embark-minimal-indicator))

   :config
   (defun delete-note (cand)
     "Delete a note."
     (interactive "fNote: ")
     (delete-file 
      (denote-get-path-by-id
       (car
        (split-string
         (format "%s" cand))))))

   (defun new-note (cand)
     "Create a note."
     (interactive "fNote: ")
     (denote cand (denote-keywords-prompt)))
   
  (keymap-set embark-general-map "n" 'new-note)
   
  :bind
  (:map minibuffer-local-map
        ("C-<tab>" . embark-act)))

(provide 'init-embark)
