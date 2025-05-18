;; -*- lexical-binding: t; t-*-
(use-package "eshell"
  :defer t
  :commands (eshell)

  :init
  (add-hook 'eshell-mode-hook
            (lambda ()
              (define-key eshell-mode-map (kbd "<up>") #'previous-line)
              (define-key eshell-mode-map (kbd "<down>") #'next-line)
              (define-key eshell-mode-map (kbd "M-<up>") #'eshell-previous-input)
              (define-key eshell-mode-map (kbd "M-<down>") #'eshell-next-input)))

  (setq eshell-prompt-function
        (lambda nil
          (concat
           (propertize "λ" 'face `(:foreground "#a3be8c" :weight bold))
           (propertize " " 'face `()))))
  (setq eshell-banner-message "")

  :config
  (defun eshell/clear ()
    "Clear terminal"
    (interactive)
    (let ((inhibit-read-only t))
      (erase-buffer))))

(provide 'init-eshell)
