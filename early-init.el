;;; -*- lexical-binding: t -*-

;;; speedup blub
(let ((default-gc-threshold gc-cons-threshold)
      (default-gc-percentage gc-cons-percentage))
  (setq gc-cons-threshold most-positive-fixnum
        default-gc-percentage 0.8)
  (add-hook 'after-init-hook
            (lambda ()
              (setq gc-cons-percentage default-gc-percentage
                    gc-cons-threshold default-gc-threshold))))

(setq load-prefer-newer t)

(setq package-enable-at-startup nil)

;; Fix for startup error
(defvar native-comp-deferred-compilation-deny-list nil)
;; The warnings buffer is obnoxious.
(setq warning-minimum-level :error)
