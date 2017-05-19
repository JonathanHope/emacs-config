;; Package configuration for counsel-projectile.

(use-package counsel-projectile
  :ensure t

  :config
  (defun my-counsel-projectile-ag (&optional options)
    "Ivy version of `projectile-ag'."
    (interactive)
    (if (projectile-project-p)
        (let* ((options
                (if current-prefix-arg
                    (read-string "options: ")
                  options))
               (ignored
                (append
                 (cl-union (projectile-ignored-files-rel) grep-find-ignored-files)
                 (cl-union (projectile-ignored-directories-rel) grep-find-ignored-directories)))
               (options
                (concat options " "
                        (mapconcat (lambda (i)
                                     (concat "--ignore " (shell-quote-argument i)))
                                   ignored
                                   " ") " --nocolor" " --vimgrep")))
          (message options)
          (counsel-ag nil
                      (projectile-project-root)
                      options
                      (projectile-prepend-project-name "ag")))
      (user-error "You're not in a project"))))

(provide 'init-counsel-projectile)
