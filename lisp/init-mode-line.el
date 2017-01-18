;; Package configuration for the mode-line.

(use-package "mode-line"
  :init 

  ;; Custom faces.
  
  (defgroup mode-line nil
    "Custom mode-line faces."
    :group 'convenience)

  (defface mode-line-inactive-face
    '()
    "Face used for the inactive modeline."
    :group 'mode-line)
  
  (defface mode-line-window-number-face
    '()
    "Face used for the window number in the modeline."
    :group 'mode-line)

  (defface mode-line-file-status-face
    '()
    "Face used for the file status in the modeline."
    :group 'mode-line)
  
  (defface mode-line-buffer-name-face
    '()
    "Face used for the buffer name in the modeline."
    :group 'mode-line)

  (defface mode-line-projectile-vc-face
    '()
    "Face used for the project name and vc branch in the modeline."
    :group 'mode-line)

  (defface mode-line-mode-face
    '()
    "Face used for the modes the modeline."
    :group 'mode-line)

  (defface mode-line-row-column-face
    '()
    "Face used for the row/column the modeline."
    :group 'mode-line)

  ;; Abstraction to figure out if mode-line is the selected window or not.
  
  (defvar mode-line-selected-window (frame-selected-window))

  (defun mode-line-set-selected-window ()
    "Set the currently selected window for use by the mode-line."
    (when (not (minibuffer-window-active-p (frame-selected-window)))
      (setq mode-line-selected-window (frame-selected-window))))

  (add-hook 'window-configuration-change-hook 'mode-line-set-selected-window)
  (add-hook 'focus-in-hook 'mode-line-set-selected-window)

  (defadvice handle-switch-frame (after mode-line-set-selected-window-after-switch-frame activate)
    "Set selected window after frame switch."
    (mode-line-set-selected-window))

  (defadvice select-window (after mode-line-select-window activate)
    "Set selected window after window switch."
    (mode-line-set-selected-window))

  (defun mode-line-selected-window-active ()
    "Check if the mode-line is in the selected window."
    (eq mode-line-selected-window (selected-window)))

  ;; Force winum to behave like the rest of the modeline.

  (eval-after-load "winum"
    '(progn
       (setq winum-mode-line-position 3)
       (defun winum-get-number-string (&optional window)
         (let* ((n (winum-get-number window))
                (s (when (numberp n) (cond ((string= (int-to-string n) "1") "❶")
                                           ((string= (int-to-string n) "2") "❷")
                                           ((string= (int-to-string n) "3") "❸")
                                           ((string= (int-to-string n) "4") "❹")
                                           ((string= (int-to-string n) "5") "❺")
                                           ((string= (int-to-string n) "6") "❻")
                                           ((string= (int-to-string n) "7") "❼")
                                           ((string= (int-to-string n) "8") "❽")
                                           ((string= (int-to-string n) "9") "❾")
                                           (t (int-to-string n))))))
           (propertize s 'face (if (mode-line-selected-window-active) 'mode-line-window-number-face 'mode-line-inactive-face))))))  
  
  ;; Bits of the mode-line.

  ;; Modeline start
  (defvar m/mode-line-start
    '(:eval (propertize "━━" 'face (if (mode-line-selected-window-active) 'mode-line 'mode-line-inactive))))
  (put 'm/mode-line-start 'risky-local-variable t)
  
  ;; Start of a section.
  (defvar m/section-start
    '(:eval (propertize "⬢ " 'face (if (mode-line-selected-window-active) 'mode-line 'mode-line-inactive))))
  (put 'm/section-start 'risky-local-variable t)

  ;; End of a section.
  (defvar m/section-end
    '(:eval (propertize " ⬢━━" 'face (if (mode-line-selected-window-active) 'mode-line 'mode-line-inactive))))
  (put 'm/section-end 'risky-local-variable t)

  ;; The number tied to the window the buffer is residing in.
  (defvar m/window-number-mode-line
    '(:eval ""))
  (put 'm/window-number-mode-line 'risky-local-variable t)

  ;; Whether the file has been edited since hte last save.
  (defvar m/file-status-mode-line
    '(:eval
      (propertize (if (buffer-modified-p) "● " "") 'face (if (mode-line-selected-window-active) 'mode-line-file-status-face 'mode-line-inactive-face))))
  (put 'm/file-status-mode-line 'risky-local-variable t)
  
  ;; The name of the file being edited.
  (defvar m/filename-mode-line
    '(:eval (propertize "%b" 'face (if (mode-line-selected-window-active) 'mode-line-buffer-name-face 'mode-line-inactive-face))))
  (put 'm/filename-mode-line 'risky-local-variable t)

  ;; What project we are in.
  (defvar m/projectile-mode-line
    '(:propertize
      (:eval (if (projectile-project-p)
                 (propertize (projectile-project-name) 'face (if (mode-line-selected-window-active) 'mode-line-projectile-vc-face 'mode-line-inactive-face))
               (propertize "N/A" 'face (if (mode-line-selected-window-active) 'mode-line-projectile-vc-face 'mode-line-inactive-face))))))
  (put 'm/custom-projectile-mode-line 'risky-local-variable t)

  ;; The current major mode.
  (defvar m/major-mode-mode-line
    '(:eval (propertize mode-name 'face (if (mode-line-selected-window-active) 'mode-line-mode-face 'mode-line-inactive-face))))
  (put 'm/major-mode-mode-line 'risky-local-variable t)
  
  ;; The current row and column being edited.
  (defvar m/row-column-mode-line
    '(:eval 
      (concat (propertize "%01l" 'face (if (mode-line-selected-window-active) 'mode-line-row-column-face 'mode-line-inactive-face))
              (propertize "," 'face (if (mode-line-selected-window-active) 'mode-line-row-column-face 'mode-line-inactive-face))
              (propertize "%01c" 'face (if (mode-line-selected-window-active) 'mode-line-row-column-face 'mode-line-inactive-face)))))
  (put 'm/row-column-mode-line 'risky-local-variable t)
   
  ;; Modeline end
  (defvar m/mode-line-end
    '(:eval (propertize (make-string 400 ?━) 'face (if (mode-line-selected-window-active) 'mode-line 'mode-line-inactive))))
  (put 'm/mode-line-end 'risky-local-variable t)
  
  ;; The mode-line.

  (setq-default mode-line-format
                (list

                 m/mode-line-start
                 
                 m/section-start         
                 m/window-number-mode-line
                 m/section-end
                 
                 m/section-start
                 m/file-status-mode-line
                 m/filename-mode-line
                 m/section-end

                 m/section-start
                 m/projectile-mode-line
                 m/section-end

                 m/section-start
                 m/major-mode-mode-line
                 m/section-end

                 m/section-start
                 m/row-column-mode-line
                 m/section-end
                 
                 m/mode-line-end)))

(provide 'init-mode-line)
