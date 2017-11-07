;; Package configuration for cmake-mode.

(use-package cmake-mode
  :ensure t
  :defer t

  :mode ("CMakeLists\\.txt\\'" "\\.cmake\\'"))

(provide 'init-cmake-mode)
