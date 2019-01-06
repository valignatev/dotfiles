(setq ring-bell-function 'ignore
      x-gtk-use-system-tooltips nil
      use-dialog-box nil
      echo-keystrokes 0.5)

(setq mouse-wheel-scroll-amount '(1)
      mouse-wheel-progressive-speed nil
      scroll-conservatively 101)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode 0)
(show-paren-mode t)

(winner-mode t)

(defalias 'yes-or-no-p 'y-or-n-p)
(setq confirm-kill-processes nil)

(desktop-save-mode t)
(save-place-mode t)

(setq gnutls-verify-error t)
(setq tls-checktrust t)
(setq straight-use-package-by-default t)
;; Straight
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
;; use-package
(straight-use-package 'use-package)

(setq vj/font-name "Hack")
(defcustom vj/font-size 13 "My default font size")
(defun set-frame-font-size (&optional font-size)
  "Change fram font size to FONT-SIZE.
If no FONT-SIZE provided, reset the font size to its default variable."
  (let ((font-size
	 (or font-size
	     (car (get 'vj/font-size 'standard-value)))))
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

(add-hook 'after-init-hook 'reset-frame-font)

(dolist
    (file
     (directory-files
      (concat (expand-file-name user-emacs-directory) "config")
      t
      directory-files-no-dot-files-regexp
      )
     )
  (load file))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#d2ceda" "#f2241f" "#67b11d" "#b1951d" "#3a81c3" "#a31db1" "#21b8c7" "#655370"])
 '(custom-enabled-themes nil)
 '(custom-safe-themes
   (quote
    ("bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" default)))
 '(pdf-view-midnight-colors (quote ("#655370" . "#fbf8ef")))
 '(vj/font-size 13))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
