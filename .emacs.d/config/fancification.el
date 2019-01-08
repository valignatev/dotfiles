;;; -*- lexical-binding: t -*-

(global-hl-line-mode t)

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
