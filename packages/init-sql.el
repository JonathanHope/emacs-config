;; Package configuration for sql.

(use-package sql
  :defer t

  :bind
  (:map  sql-mode-map
    ("C-<tab>" . sql-hydra/body))

  :config
  (defcustom sql-ss-program "jisql"
    "Command to start Microsoft SQL Server by jisql."
    :type 'file
    :group 'SQL)

  (defcustom sql-ss-login-params '(user password server database)
    "Login parameters to needed to connect to Microsoft SQL Server."
    :type 'sql-login-params
    :group 'SQL)

  (defcustom sql-ss-options '("sqljdbc.jar" "com.microsoft.sqlserver.jdbc.SQLServerDriver")
    "List of additional options for `sql-ss-program'."
    :type '(repeat string)
    :group 'SQL)

  (defun sql-comint-ss (product options)
    "Create comint buffer and connect to SQL Server."
    (let ((params
           (append
            (if (not (string= "" sql-user))
                (list sql-user))
            (if (not (string= "" sql-password))
                (list sql-password))
            (if (not (string= "" sql-server))
                (list sql-server))
            (if (not (string= "" sql-database))
                (list sql-database))
            options)))
  (sql-comint product params)))

  (defun sql-ss (&optional buffer)
    "Run Microsoft SQL Server as an inferior process."
    (interactive "P")
    (sql-product-interactive 'ss buffer))

  (eval-after-load "sql"
    '(sql-add-product
      'ss "SQL Server"
      :font-lock sql-mode-ms-font-lock-keywords
      :sqli-program 'sql-ss-program
      :sqli-options 'sql-ss-options
      :sqli-login 'sql-ss-login-params
      :sqli-comint-func 'sql-comint-ss
      :prompt-regexp "^[0-9]* >"
      :prompt-length 5)))

(provide 'init-sql)
