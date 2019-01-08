;;; -*- lexical-binding: t -*-

(defvar file-name-handler-alist-old file-name-handler-alist)

;; Make startup faster
(setq package-enable-at-startup nil
      file-name-handler-alist nil
      message-log-max 16384
      gc-cons-threshold 402653184
      gc-cons-percentage 0.6
      auto-window-vscroll nil
      package--init-file-ensured t)

(add-hook 'after-init-hook
          `(lambda ()
             (setq file-name-handler-alist file-name-handler-alist-old
                   gc-cons-threshold 800000
                   gc-cons-percentage 0.1)) t)

(setq ring-bell-function 'ignore
      x-gtk-use-system-tooltips nil
      use-dialog-box nil
      echo-keystrokes 0.5)

;; Scrolling
(setq mouse-wheel-scroll-amount '(1)
      mouse-wheel-progressive-speed nil
      scroll-conservatively 101
      hscroll-margin 1
      hscroll-step 1
      scroll-preserve-screen-position t)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode 0)
(show-paren-mode t)
(column-number-mode t)

(global-visual-line-mode 1)
(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))
(fringe-mode 16)

(winner-mode t)

(defalias 'yes-or-no-p 'y-or-n-p)
(setq confirm-kill-processes nil)

(desktop-save-mode t)
(save-place-mode t)

(setq gnutls-verify-error t)
(setq tls-checktrust t)

;; Straight
(setq straight-use-package-by-default t)
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

;; Load all files from my ~/.emacs.d/config directory
;; It doesn't support nested dirs
(dolist
    (file
     (directory-files
      (concat (expand-file-name user-emacs-directory) "config")
      t
      "^.[^#].+el$"))
  (load file))

;; Load automatically generated custom garbage
(setq custom-file
      (concat (file-name-directory user-init-file) "custom-variables.el"))
(when (file-exists-p custom-file)
  (load custom-file))
