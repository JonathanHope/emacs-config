(setq old-default-directory default-directory)

(defun revert-default-directory () 
	(setq default-directory old-default-directory))

;; TODO: Start in org mde in note directory.
(defun startup ()
	(make-directory "~/Notes/" :parents)
	(if (not (file-exists-p "~/Notes/.projectile"))
		(write-region "" nil "~/Notes/.projectile")
		nil)
	(setq default-directory "~/Notes/")
	(org-mode)
	)

(startup)