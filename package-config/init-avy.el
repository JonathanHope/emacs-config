;; -*- lexical-binding: t; -*-
(use-package avy
  :straight t
  :defer t

  :config
  (defun avy-action-embark (pt)
    (unwind-protect
        (save-excursion
          (goto-char pt)
          (embark-act))
      (select-window
       (cdr (ring-ref avy-ring 0))))
    t)

  (defun avy-action-mark-to-char (pt)
    (activate-mark)
    (goto-char pt))

  (setf (alist-get ?. avy-dispatch-alist) 'avy-action-embark)
  (setf (alist-get ?  avy-dispatch-alist) 'avy-action-mark-to-char))

(provide 'init-avy)
