(use-package treesit-parser-manager
  :straight (treesit-parser-manager :host codeberg :repo "ckruse/treesit-parser-manager" :files ("*.el"))
  :commands (treesit-parser-manager-install-grammars
             treesit-parser-manager-update-grammars
             treesit-parser-manager-install-or-update-grammars
             treesit-parser-manager-remove-grammar)
  :custom
  (treesit-parser-manager-grammars
   '(("https://github.com/tree-sitter/tree-sitter-typescript"
      ("tree-sitter-typescript/tsx" "tree-sitter-typescript/typescript"))))
  :config
  (setq treesit-extra-load-path (list (expand-file-name "tree-sit" user-emacs-directory)))
  :hook (emacs-startup . treesit-parser-manager-install-grammars))

(provide 'init-treesit-parser-manager)
