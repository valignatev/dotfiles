;; Interface
(setq inhibit-startup-message t
      inhibit-splash-screen t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(show-paren-mode 1)
(setq column-number-mode 1)
(blink-cursor-mode 0)
(global-visual-line-mode 1)
(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))
(defalias 'yes-or-no-p 'y-or-n-p)

;; Bind meta to left command key and then unset Command+Space
;; because I use it for switching layout
(setq x-super-keysym 'meta)
(global-unset-key (kbd "M-SPC"))
(setq scroll-conservatively 101)
;; https://emacs.stackexchange.com/q/28736/13740
;; Also, jwiegley does thi s
(setq auto-window-vscroll nil)

;; Straight package manager bootstrap and setup
(let ((bootstrap-file (concat user-emacs-directory "straight/repos/straight.el/bootstrap.el"))
      (bootstrap-version 3))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Set frame font
(setq vj/font-name "Hack"
      vj/font-size 12)

(defun vj/set-frame-font-size (&optional font-size)
  "Sets font size for all frames. Default is vj/font-size"
  (interactive (list
		(read-number "number: " vj/font-size)))
  (let ((font-size (or font-size vj/font-size)))
    (set-frame-font (format "%s %d" vj/font-name font-size) nil t)))

(vj/set-frame-font-size)

;; Theme
;; TODO may be check if custom themes is empty and then load?
(straight-use-package 'spacemacs-theme)
(setq spacemacs-theme-comment-bg nil)
(setq spacemacs-theme-comment-italic t)
;; If I want to use original comment color with italics together,
;; I should uncomment these 2 lines.
;; See https://github.com/nashamri/spacemacs-theme/issues/104
;; (custom-set-variables '(spacemacs-theme-custom-colors
;; 			'((comment-light . "#2aa1ae"))))
(global-hl-line-mode t)

(defvar vj/themes
  '((light . #'spacemacs-light)
    (dark . #'spacemacs-dark)))

(defvar vj/theme-type 'light)

(defun vj/toggle-theme ()
  (interactive)
  (mapcar #'disable-theme custom-enabled-themes)
  (if (eq vj/theme-type 'light)
      (setq vj/theme-type 'dark)
    (setq vj/theme-type 'light))
    (load-theme (eval (cdr (assoc vj/theme-type vj/themes))) t))

;; Reset to default emacs theme
(global-set-key (kbd "C-c <f6>") (lambda ()
				   (interactive)
				   (mapcar #'disable-theme custom-enabled-themes)))

(global-set-key (kbd "<f6>") 'vj/toggle-theme)
(load-theme (eval (cdr (assoc vj/theme-type vj/themes))) t)

;; Buffers and backups
(desktop-save-mode t)
(global-auto-revert-mode t)
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
(setq undo-tree-auto-save-history t)
(setq undo-tree-history-directory-alist
      `((".*" . ,(concat user-emacs-directory "undo/"))))

;; Security
(setq gnutls-verify-error t)
(setq tls-checktrust t)

;; Minibuffer (find-file, M-x etc)
(ido-mode 1)
(setq ido-enable-flex-matching t)
(straight-use-package 'ido-vertical-mode)
(ido-vertical-mode t)
(setq ido-vertical-define-keys 'C-n-and-C-p-only)
(straight-use-package 'smex)
(global-set-key (kbd "M-x") 'smex)

;; Evil
(straight-use-package 'evil)
;; Bind it to something silly since I use C-z to run terminal
(setq evil-vsplit-window-right t)
(setq evil-split-window-below t)
;; (setq evil-split-window-below t)
(setq evil-toggle-key "C-c C-z")
(setq evil-want-C-u-scroll t)
(evil-mode 1)
(modify-syntax-entry ?_ "w")
(straight-use-package 'evil-surround)
(global-evil-surround-mode t)
(straight-use-package 'evil-ediff)
(evil-ediff-init)
(straight-use-package 'evil-leader)
(global-evil-leader-mode)
(evil-leader/set-leader "<SPC>")
;; Generalize it
(evil-leader/set-key
  "t" '(lambda () (interactive)(shell-command "npm test")))
;; Evil can do this:
;; (evil-leader/set-key-for-mode 'emacs-lisp-mode "b" 'byte-compile-file)

;; Magit
(straight-use-package 'magit)
(global-set-key (kbd "C-c g") 'magit-status)
(straight-use-package 'evil-magit)
(evil-magit-init)

;; Projectile
(straight-use-package 'projectile)
(projectile-mode 1)

;; Modeline
;; TODO: moody master now autoloads commands
;;(straight-use-package 'moody)
;;(require 'moody )
;; (require 'moody "~/.emacs.d/straight/repos/moody/moody.el")
;;(setq x-underline-at-descent-line t)
;;(moody-replace-mode-line-buffer-identification)
;;(moody-replace-vc-mode)
(straight-use-package 'minions)
(minions-mode t)

;; Shell (if I accidentally end up there)
(setq comint-prompt-read-only t)
(setq comint-input-ignoredups t)

;; Parse ANSI escape characters in *Shell Command Output* buffer
(defun parse-ansi-for-shell-command-output (message-or-buffer)
  (let ((buf message-or-buffer))
    (and (bufferp buf)
	 (string= (buffer-name buf) "*Shell Command Output*")
	 (with-current-buffer buf
	   (ansi-color-apply-on-region (point-min) (point-max))))))
(advice-add 'display-message-or-buffer :before #'parse-ansi-for-shell-command-output)


;; Term (I actually use it)
;; This section is a total mess
;; Set normal mote for terminal-mode so we can have
;; block cursor. May be there is a better way
(evil-set-initial-state 'term-mode 'normal)
(setq term-input-ignoredups t)
;; Original term function constantly asks for my shell
(defun vj/term ()
  (interactive)
  (term shell-file-name))
;; (evil-set-initial-state 'term-mode 'emacs)
(global-set-key (kbd "C-z") 'vj/term)
;; I need to call for disabling evil-mode for both
;; vj/term and term-char-mode because in first scenario
;; we should wait until we in *terminal* buffer before
;; disabling evil
(advice-add 'vj/term :after #'turn-off-evil-mode)
(advice-add 'term-char-mode :after #'turn-off-evil-mode)
(advice-add 'term-line-mode :after #'turn-on-evil-mode)
(add-hook 'term-mode-hook
	  (lambda ()
	    (define-key term-raw-map (kbd "M-x") 'smex)))

;; Parens
(electric-pair-mode t)

;; editorconfig
(straight-use-package 'editorconfig)
(editorconfig-mode t)

;; Python
(defun vj/comint-clear-buffer (&optional buffer-or-name)
  "Same as plain `comint-clear-buffer' but can pass buffer or name of buffer.
I often work with two splits - the code and inferior shell. With this function
I don't need to switch to another window with comint buffer to clear it"
  (interactive)
  (let ((buffer-or-name (or buffer-or-name "")))
    (let ((buf (or (get-buffer buffer-or-name) (current-buffer))))
      (with-current-buffer buf
	(comint-clear-buffer)))))

;; Scroll to bottom of inferior python REPL
;; and kill running Python process without confirmation.
(add-hook 'inferior-python-mode-hook
	  (lambda ()
	    (setq comint-move-point-for-output t)
	    (set-process-query-on-exit-flag (get-process "Python") nil)))

(add-hook 'python-mode-hook
	  (lambda ()
	    (local-set-key (kbd "C-c M-o")
			   (lambda ()
			     (interactive)
			     (vj/comint-clear-buffer "*Python*")))))
