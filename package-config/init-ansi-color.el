;; -*- lexical-binding: t; -*-
(use-package ansi-color
  :defer t

  :commands (ansi-color-make-color-map)

  :init
  (setq ansi-color-names-vector
        (vector "#2b303b"         ; "black"
	              "#bf616a"         ; "red3"
	              "#a3be8c"         ; "green3"
	              "#ebcb8b"         ; "yellow3"
	              "#65737e"         ; "blue2"
	              "#b48ead"         ; "magenta3"
	              "#a3c6d0"         ; "cyan3"
	              "#c0c5ce"         ; "gray90"
	              ))

  (setq ansi-color-map (ansi-color-make-color-map)))

(provide 'init-ansi-color)
