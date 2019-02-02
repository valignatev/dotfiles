;;; -*- lexical-binding: t -*-

(global-hl-line-mode t)

(global-visual-line-mode 1)
(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))
(fringe-mode 16)

(setq vj/font-name "Hack")
(defcustom vj/font-size 12 "My default font size")

(defun set-frame-font-size (&optional font-size)
  "Change fram font size to FONT-SIZE.
If no FONT-SIZE provided, reset the font size to its default variable."
  (let ((font-size
	     (or font-size
	         (eval (car (get 'vj/font-size 'standard-value))))))
    (customize-set-variable 'vj/font-size font-size)
    (set-frame-font
     (format "%s %d" vj/font-name font-size) nil t)))

(defun increase-frame-font ()
  "Increase frame font by one."
  (interactive)
  (set-frame-font-size (+ vj/font-size 1)))

(defun decrease-frame-font ()
  "Decrease frame font by one."
  (interactive)
  (set-frame-font-size (- vj/font-size 1)))

(defun reset-frame-font ()
  "Reset frame font to its default value."
  (interactive)
  (set-frame-font-size))

;; Probably good case for a hydra
(global-set-key (kbd "C-x C-0") 'reset-frame-font)
(global-set-key (kbd "C-x C--") 'decrease-frame-font)
(global-set-key (kbd "C-x C-=") 'increase-frame-font)
(global-set-key (kbd "C-x C-+") 'text-scale-adjust)

(add-hook 'after-init-hook 'reset-frame-font)

(use-package spacemacs-theme
  :defer t
  :custom
  (spacemacs-theme-comment-bg nil)
  (spacemacs-theme-comment-italic t))
;; If I ever want to use original comment color with italics together,
;; I should uncomment these 2 lines.
;; See https://github.com/nashamri/spacemacs-theme/issues/104
;; (custom-set-variables '(spacemacs-theme-custom-colors
;;                         '((comment-light . "#2aa1ae"))))

(use-package heaven-and-hell
  :init
  (setq heaven-and-hell-theme-type 'light)
  (setq heaven-and-hell-themes
	'((light . spacemacs-light)
	  (dark . spacemacs-dark)))
  :hook (after-init . heaven-and-hell-init-hook)
  :bind (("C-c <f6>" . heaven-and-hell-load-default-theme)
	 ("<f6>" . heaven-and-hell-toggle-theme)))

(use-package all-the-icons
  :config
  ;; all-the-icons doesn't work without font-lock+
  ;; And font-lock+ doesn't have autoloads
  (use-package font-lock+
    :straight (:host github :repo "emacsmirror/font-lock-plus")
    :config (require 'font-lock+)))

(use-package which-key
  :init (which-key-mode))

(use-package company-box
  :after (company all-the-icons)
  :hook (company-mode . company-box-mode)
  :config
  (setq
   company-box-show-single-candidate t
   company-box-icons-elisp
   (list (all-the-icons-material "functions"                  :height 0.8 :face 'all-the-icons-red)
         (all-the-icons-material "check_circle"               :height 0.8 :face 'all-the-icons-blue)
         (all-the-icons-material "stars"                      :height 0.8 :face 'all-the-icons-orange)
         (all-the-icons-material "format_paint" :height 0.8 :face 'all-the-icons-pink))
   company-box-icons-lsp
   `((1  . ,(all-the-icons-material "text_fields"              :height 0.8 :face 'all-the-icons-green)) ; text
     (2  . ,(all-the-icons-material "functions"                :height 0.8 :face 'all-the-icons-red))   ; method
     (3  . ,(all-the-icons-material "functions"                :height 0.8 :face 'all-the-icons-red))   ; function
     (4  . ,(all-the-icons-material "functions"                :height 0.8 :face 'all-the-icons-red))   ; constructor
     (5  . ,(all-the-icons-material "functions"                :height 0.8 :face 'all-the-icons-red))   ; field
     (6  . ,(all-the-icons-material "adjust"                   :height 0.8 :face 'all-the-icons-blue))  ; variable
     (7  . ,(all-the-icons-material "class"                    :height 0.8 :face 'all-the-icons-red))   ; class
     (8  . ,(all-the-icons-material "settings_input_component" :height 0.8 :face 'all-the-icons-red))   ; interface
     (9  . ,(all-the-icons-material "view_module"              :height 0.8 :face 'all-the-icons-red))   ; module
     (10 . ,(all-the-icons-material "settings"                 :height 0.8 :face 'all-the-icons-red))   ; property
     (11 . ,(all-the-icons-material "straighten"               :height 0.8 :face 'all-the-icons-red))   ; unit
     (12 . ,(all-the-icons-material "filter_1"                 :height 0.8 :face 'all-the-icons-red))   ; value
     (13 . ,(all-the-icons-material "plus_one"                 :height 0.8 :face 'all-the-icons-red))   ; enum
     (14 . ,(all-the-icons-material "filter_center_focus"      :height 0.8 :face 'all-the-icons-red))   ; keyword
     (15 . ,(all-the-icons-material "short_text"               :height 0.8 :face 'all-the-icons-red))   ; snippet
     (16 . ,(all-the-icons-material "color_lens"               :height 0.8 :face 'all-the-icons-red))   ; color
     (17 . ,(all-the-icons-material "insert_drive_file"        :height 0.8 :face 'all-the-icons-red))   ; file
     (18 . ,(all-the-icons-material "collections_bookmark"     :height 0.8 :face 'all-the-icons-red))   ; reference
     (19 . ,(all-the-icons-material "folder"                   :height 0.8 :face 'all-the-icons-red))   ; folder
     (20 . ,(all-the-icons-material "people"                   :height 0.8 :face 'all-the-icons-red))   ; enumMember
     (21 . ,(all-the-icons-material "pause_circle_filled"      :height 0.8 :face 'all-the-icons-red))   ; constant
     (22 . ,(all-the-icons-material "streetview"               :height 0.8 :face 'all-the-icons-red))   ; struct
     (23 . ,(all-the-icons-material "event"                    :height 0.8 :face 'all-the-icons-red))   ; event
     (24 . ,(all-the-icons-material "control_point"            :height 0.8 :face 'all-the-icons-red))   ; operator
     (25 . ,(all-the-icons-material "class" :height 0.8 :face 'all-the-icons-red))))
  )

(use-package org-bullets
  :hook (org-mode . org-bullets-mode))
