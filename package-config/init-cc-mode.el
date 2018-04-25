;; Package configuration for cc-mode.

(use-package cc-mode
  :defer t

  :mode (("\\.h$" . c++-mode)
         ("\\.hpp$" . c++-mode)
         ("\\.cpp$" . c++-mode)
         ("\\.tpp$" . c++-mode))

  :bind
  (:map c++-mode-map
        ("M-o". ff-find-other-file)
        ("C-<tab>" . cpp-hydra/body))

  :init
  (add-hook 'c-mode-hook (lambda () (c-set-style "k&r")))
  ;; (add-hook 'c++-mode-hook 'lsp-cquery-enable)
  ;; (add-hook 'lsp-after-open-hook 'lsp-enable-imenu)
  ;; (add-hook 'c++-mode-hook 'lsp-clangd-enable)
  ;; (unbind-key "<f12>" global-map)
  ;; (unbind-key "S-<f12>" global-map)

  ;; Substitute a wide variety of characters for prettier characters.
  ;; (add-hook 'c++-mode-hook
  ;;           (lambda ()
  ;;             (setq prettify-symbols-unprettify-at-point 'right-edge)
  ;;             (setq prettify-symbols-unprettify-at-point 'right-edge)

  ;;             (defconst pragmatapro-prettify-symbols-alist
  ;;               (mapcar (lambda (s)
  ;;                         `(,(car s)
  ;;                           .
  ;;                           ,(vconcat
  ;;                             (apply 'vconcat
  ;;                                    (make-list
  ;;                                     (- (length (car s)) 1)
  ;;                                     (vector (decode-char 'ucs #X0020) '(Br . Bl))))
  ;;                             (vector (decode-char 'ucs (cadr s))))))
  ;;                       '(("==" #XE821)
  ;;                         ("!=" #XE721)
  ;;                         ("||" #XE881)
  ;;                         ("&&" #XE761)
  ;;                         ("++" #XE790)
  ;;                         ("--" #XE7A0)
  ;;                         (">=" #XE841)
  ;;                         ("<=" #XE7E8)
  ;;                         (">>" #XE842)
  ;;                         ("<<" #XE7E5)
  ;;                         ("->" #XE7A4)
  ;;                         ("::" #XE7D0)
  ;;                         ("/=" #XE7C3)
  ;;                         ("*=" #XE781)
  ;;                         ("+=" #XE792)
  ;;                         ("-=" #XE7A3))))
  ;;             (dolist (alias pragmatapro-prettify-symbols-alist)
  ;;               (push alias prettify-symbols-alist))))

  :config

  ;; (require 'lsp-mode)
  ;; (require 'lsp-imenu)

  ;; (setq auto-save-default nil)(setq lsp-enable-indentation nil)
  ;; (lsp-define-stdio-client
  ;;  lsp-clangd "cpp" #'projectile-project-root
  ;;  `("clangd"))


  ;; (require 'lsp-ui-flycheck)
  ;; (with-eval-after-load 'lsp-mode
  ;;   (add-hook 'lsp-after-open-hook (lambda () (lsp-ui-flycheck-enable 1))))
  ;; (require 'company-lsp)
  ;; (push 'company-lsp company-backends)
  ;; (setq company-transformers nil company-lsp-async t company-lsp-cache-candidates nil company-lsp-enable-recompletion t)
  ;; (require 'cquery)
  ;; (setq cquery-executable "~/.emacs.d/cquery/cquery.exe")
  ;; (setq cquery-extra-init/-params '(:extraClangArguments ("--driver-mode=cl")))
  ;; (setq cquery-extra-init-params '(:index (:comments 2) :cacheFormat "msgpack" :completion (:detailedLabel t)))
  )

(provide 'init-cc-mode)
