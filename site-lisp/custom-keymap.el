;; This function will define a custom global keymap that has precence over almost everything.

(defun configure-custom-keymap (shortcuts)
  (defvar my-keys-minor-mode-map
    (let ((map (make-keymap)))
      (dolist (shortcut shortcuts) 
        (define-key map (kbd (car shortcut)) (car (last shortcut))))
      (define-minor-mode my-keys-minor-mode t " my-keys" 'my-keys-minor-mode-map)
      (my-keys-minor-mode 1)
      (defconst my-minor-mode-alist (list (cons 'my-keys-minor-mode map)))
      (setf emulation-mode-map-alists '(my-minor-mode-alist)))))

(provide 'custom-keymap)