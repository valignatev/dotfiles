;;; -*- lexical-binding: t -*-

(setq ring-bell-function 'ignore        ; don't beep
      x-gtk-use-system-tooltips nil     ; no gui popups
      use-dialog-box nil                ; no gui popups dammit!
      echo-keystrokes 0.5               ; echo keystrokes faster
      confirm-kill-processes nil        ; just kill the process
      disabled-command-function nil)    ; enable all commands

(menu-bar-mode -1)                      ; menu-bar is gone
(tool-bar-mode -1)                      ; tool-bar is gone
(scroll-bar-mode -1)                    ; scroll-bar is gone
(blink-cursor-mode 0)                   ; stop blinking on me!
(show-paren-mode t)                     ; highlight matching parens
(column-number-mode t)                  ; show column numbers
(global-auto-revert-mode t)             ; reload changes from the disk

;; Scrolling
(setq mouse-wheel-scroll-amount '(1)    ; scroll gentle
      mouse-wheel-progressive-speed nil ; don't accelerate
      scroll-conservatively 101         ; don't jump to the middle of screen
      hscroll-margin 1                  ; don't you scroll that early!
      hscroll-step 1                    ; but scroll just a bit
      scroll-preserve-screen-position t) ; try to keep cursor in its position

(winner-mode t)
(defalias 'yes-or-no-p 'y-or-n-p)

(desktop-save-mode t)
(save-place-mode t)

;; Security hype
(setq gnutls-verify-error t
      tls-checktrust t)

;; Disk space is cheap. Save lots. (c) Sacha Chua
;; Backups
(setq backup-directory-alist '(("." . "~/.emacs.d/backups"))
      delete-old-versions -1
      version-control t
      vc-make-backup-files t
      auto-save-list-file-prefix "~/.emacs.d/autosave/"
      auto-save-file-name-transforms '((".*" "~/.emacs.d/autosave/" t))
      undo-tree-auto-save-history t
      undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo/")))

;; But don't create stupid lockfiles
(setq create-lockfiles nil)

;; History
(setq savehist-file "~/.emacs.d/savehist"
      history-length t
      history-delete-duplicates t
      savehist-save-minibuffer-history 1
      savehist-additional-variables
      '(kill-ring
        search-ring
        regexp-search-ring)
      recentf-max-saved-items 50)
(savehist-mode 1)
(recentf-mode 1)
