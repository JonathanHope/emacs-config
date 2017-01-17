;; Package configuration for polymode

(use-package polymode
  :ensure t

  :mode (("\\.html$" . poly-html-mode))
  
  :config 
  (defcustom pm-host/html
    (pm-bchunkmode "html" :mode 'html-mode)
    "html host chunckmode"
    :group 'hostmodes
    :type 'object)

  (defcustom pm-inner/html-css
    (pm-hbtchunkmode "html css block"
                     :mode 'css-mode
                     :head-mode 'html-mode
                     :tail-mode 'html-mode
                     :head-reg "<style type=\"text/css\".*>"
                     :tail-reg  "</style>"
                     :adjust-face nil)
    "html css chunck"
    :group 'innermodes
    :type 'object)

  (defcustom pm-inner/html-js
    (pm-hbtchunkmode "html css block"
                     :mode 'js-mode
                     :head-mode 'html-mode
                     :tail-mode 'html-mode
                     :head-reg "<script type=\"text/javascript\".*>"
                     :tail-reg  "</script>"
                     :adjust-face nil)
    "html js chunck"
    :group 'innermodes
    :type 'object)

  (defcustom pm-poly/html2
    (pm-polymode-multi "html"
                       :hostmode 'pm-host/html
                       :innermodes '(pm-inner/html-css
                                     pm-inner/html-js))

    "html polymode"
    :group 'polymodes
    :type 'object)

  (define-polymode poly-html-mode pm-poly/html2))

(provide 'init-polymode)
