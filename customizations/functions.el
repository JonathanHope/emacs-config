;; This is for top level functions that are not related to a particular package.

(defun revert-default-directory ()
  "Revert the default directory to the directory that emacs was started in."
  (setq default-directory old-default-directory))

(defun startup ()
  "Custom startup function. Defaults to org mode in the notes directory."
  (make-directory notes-directory :parents)
  (make-directory scratch-directory :parents)

  (let ((projectile-file (concat notes-directory ".projectile")))
    (if (not (file-exists-p projectile-file))
        (write-region "" nil projectile-file)
      nil))

  (dolist (scratch-file scratch-files) 
    (let ((scratch-file-path (concat scratch-directory scratch-file)))
      (if (not (file-exists-p scratch-file-path))
          (write-region "" nil scratch-file-path)
        nil)))
  
  (setq default-directory notes-directory)
  (org-mode))