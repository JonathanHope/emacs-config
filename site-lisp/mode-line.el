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
  "Face used for the modes in the modeline."
  :group 'mode-line)

(defface mode-line-row-column-face
  '()
  "Face used for the row/column in the modeline."
  :group 'mode-line)

(defface mode-line-scroll-bar-face
  '()
  "Face used for the scroll bar in the modeline."
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

;; The start of the mode line.
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

;; Start of a active only section.
(defvar m/active-section-start
  '(:eval (propertize (if (mode-line-selected-window-active) "⬢ " "") 'face (if (mode-line-selected-window-active) 'mode-line 'mode-line-inactive))))
(put 'm/active-section-start 'risky-local-variable t)

;; End of a active only section.
(defvar m/active-section-end
  '(:eval (propertize (if (mode-line-selected-window-active) " ⬢━━" "") 'face (if (mode-line-selected-window-active) 'mode-line 'mode-line-inactive))))
(put 'm/active-section-end 'risky-local-variable t)

;; This char is used to adjust the height of the mode line to have perfectly even padding.
(defvar m/adjust-height
  (propertize "\u200b" 'display '((raise -0.08) (height 1.1))))
(put 'm/adjust-height 'risky-local-variable t)

;; The number tied to the window the buffer is residing in.
(defvar m/window-number-mode-line
  '(:eval ""))
(put 'm/window-number-mode-line 'risky-local-variable t)

;; Whether the file has been edited since the last save.
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
  '(:eval  (if (mode-line-selected-window-active)
               (if (projectile-project-p)
                   (propertize (projectile-project-name) 'face (if (mode-line-selected-window-active) 'mode-line-projectile-vc-face 'mode-line-inactive-face))
                 (propertize "N/A" 'face (if (mode-line-selected-window-active) 'mode-line-projectile-vc-face 'mode-line-inactive-face)))
             "")))
(put 'm/custom-projectile-mode-line 'risky-local-variable t)

;; The current major mode.
(defvar m/major-mode-mode-line
  '(:eval (if (mode-line-selected-window-active) (propertize "%m" 'face (if (mode-line-selected-window-active) 'mode-line-mode-face 'mode-line-inactive-face)) "")))
(put 'm/major-mode-mode-line 'risky-local-variable t)

(defun what-line-no-print ()
  "Get the current buffer line number and narrowed line number of point."
  (let ((start (point-min))
        (n (line-number-at-pos)))
    (if (= start 1)
        n
      (save-excursion
        (save-restriction
          (widen)
          (+ n (line-number-at-pos start) -1))))))

(defun get-scroll-percent ()
  "Calculate how far down the buffer the cursor is by line."
  (let ((current-line (- (what-line-no-print) 1))
        (max-line (count-lines (point-min) (point-max))))
    (if (= 0 max-line)
        0
      (* 100.0 (/ (float current-line) (float max-line))))))

(defun get-scroll-bar ()
  "Build an text based horizontal scroll bar."
  (let* ((p (get-scroll-percent))
         (s (cond ((and (> p 0) (<= p 10)) "❰━         ❱")
                  ((and (> p 10) (<= p 20)) "❰━━        ❱")
                  ((and (> p 20) (<= p 30)) "❰━━━       ❱")
                  ((and (> p 30) (<= p 40)) "❰━━━━      ❱")
                  ((and (> p 40) (<= p 50)) "❰━━━━━     ❱")
                  ((and (> p 50) (<= p 60)) "❰━━━━━━    ❱")
                  ((and (> p 60) (<= p 70)) "❰━━━━━━━   ❱")
                  ((and (> p 70) (<= p 80)) "❰━━━━━━━━  ❱")
                  ((and (> p 80) (<= p 90)) "❰━━━━━━━━━ ❱")
                  ((> p 90) "❰━━━━━━━━━━❱")
                  (t "❰          ❱"))))
    (propertize s 'face (if (mode-line-selected-window-active) 'mode-line-scroll-bar-face 'mode-line-inactive-face))))

;; A horizontal scroll line.
(defvar m/scroll-bar-mode-line
  '(:eval (if (mode-line-selected-window-active) (get-scroll-bar) "")))
(put 'm/scroll-bar-mode-line 'risky-local-variable t)

;; The current row and column being edited.
(defvar m/row-column-mode-line
  '(:eval
    (if (mode-line-selected-window-active)
        (concat (propertize "%01l" 'face (if (mode-line-selected-window-active) 'mode-line-row-column-face 'mode-line-inactive-face))
                (propertize "," 'face (if (mode-line-selected-window-active) 'mode-line-row-column-face 'mode-line-inactive-face))
                (propertize "%01c" 'face (if (mode-line-selected-window-active) 'mode-line-row-column-face 'mode-line-inactive-face)))
      "")))
(put 'm/row-column-mode-line 'risky-local-variable t)

;; The end of the mode line.
(defvar m/mode-line-end
  '(:eval (propertize (make-string 400 ?━) 'face (if (mode-line-selected-window-active) 'mode-line 'mode-line-inactive))))
(put 'm/mode-line-end 'risky-local-variable t)

;; The mode-line.

(setq-default mode-line-format
              (list
               m/mode-line-start

               m/section-start
               m/adjust-height
               m/window-number-mode-line
               m/adjust-height
               m/section-end

               m/section-start
               m/file-status-mode-line
               m/filename-mode-line
               m/section-end

               m/active-section-start
               m/projectile-mode-line
               m/active-section-end

               m/active-section-start
               m/major-mode-mode-line
               m/active-section-end

               m/active-section-start
               m/scroll-bar-mode-line
               m/active-section-end

               m/active-section-start
               m/row-column-mode-line
               m/active-section-end

               m/mode-line-end))

(provide 'mode-line)
