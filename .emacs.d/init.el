;;; init.el --- my Emacs customization.
;;;
;;; Commentary:
;; I add things here when I need 'em.
;; Custom functions are prefixed with "vj"
;;
;;; Code:

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
(fringe-mode 16)
(defalias 'yes-or-no-p 'y-or-n-p)
(setq x-gtk-use-system-tooltips nil)
(setq use-dialog-box nil)
(global-hl-line-mode t)
(winner-mode 1)
(setq confirm-kill-processes nil)
(setq echo-keystrokes 0.5)
(setq gc-cons-threshold 50000000)
(setq frame-title-format
      '("" invocation-name ": " (:eval (replace-regexp-in-string
                                        "^ +" "" (or (buffer-file-name) (buffer-name))))))
(add-hook 'prog-mode-hook (lambda ()(modify-syntax-entry ?_ "w")))
(setq savehist-file "~/.emacs.d/etc/savehist"
      history-length 150)
(savehist-mode)
(setq-default save-place t)
(setq save-place-file "~/.emacs.d/etc/saveplace")
(setq bookmark-default-file "~/.emacs.d/etc/bookmarks")

;; Scrolling
(setq mouse-wheel-scroll-amount '(1))
(setq mouse-wheel-progressive-speed nil)

;; Unset M+SPC because I use it for dmenu
(global-unset-key (kbd "M-SPC"))
(setq scroll-conservatively 101)
;; https://emacs.stackexchange.com/q/28736/13740
;; Also, jwiegley does this
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

;; Also, I want use-package
(straight-use-package 'use-package)

;; Set frame font
(defvar vj-font-name "Hack")
(defvar vj-font-size 12)

(defun vj-set-frame-font-size (&optional font-size)
  "Set font size FONT-SIZE for all frames (including modeline and minibuffer).
Default is vj-font-size"
  (interactive (list
                (read-number "number: " vj-font-size)))
  (let ((font-size (or font-size vj-font-size)))
    (set-frame-font (format "%s %d" vj-font-name font-size) nil t)))

(add-hook 'after-init-hook 'vj-set-frame-font-size)

;; Theme
(use-package spacemacs-theme
  :straight t
  :defer t
  :custom
  (spacemacs-theme-comment-bg nil)
  (spacemacs-theme-comment-italic t))
;; If I want to use original comment color with italics together,
;; I should uncomment these 2 lines.
;; See https://github.com/nashamri/spacemacs-theme/issues/104
;; (custom-set-variables '(spacemacs-theme-custom-colors
;;                         '((comment-light . "#2aa1ae"))))

(use-package heaven-and-hell
  :straight t
  :init
  (setq heaven-and-hell-theme-type 'light)
  (setq heaven-and-hell-themes
        '((light . spacemacs-light)
          (dark . spacemacs-dark)))
  :hook (after-init . heaven-and-hell-init-hook)
  :bind (("C-c <f6>" . heaven-and-hell-load-default-theme)
         ("<f6>" . heaven-and-hell-toggle-theme)))

(use-package dired-sidebar
  :straight t
  :commands (dired-sidebar-toggle-sidebar))

;; Icons for dired
(use-package all-the-icons
  :straight t
  :config
  ;; all-the-icons doesn't work without font-lock+
  ;; And font-lock+ doesn't have autoloads
  (use-package font-lock+
    :straight (:host github :repo "emacsmirror/font-lock-plus")
    :config (require 'font-lock+))
  (use-package all-the-icons-dired
    :straight t
    :hook (dired-mode . all-the-icons-dired-mode)))

;; Buffers and backups
(desktop-save-mode t)
(global-auto-revert-mode t)
(setq create-lockfiles nil)
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)
(customize-set-variable 'indent-tabs-mode nil)
(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory "backups"))))
(setq delete-old-versions t)
;; I tried to use concat (user-emacs-directory "autosave") here, but
;; it doesn't work
(setq auto-save-list-file-prefix "~/.emacs.d/autosave/")
(setq auto-save-file-name-transforms '((".*"  "~/.emacs.d/autosave/" t)))
(setq undo-tree-auto-save-history t)
(setq undo-tree-history-directory-alist
      `((".*" . ,(concat user-emacs-directory "undo/"))))
(add-hook 'focus-out-hook (lambda () (interactive) (save-some-buffers t)))
;; save when frame is closed
(add-hook 'delete-frame-functions (lambda () (interactive) (save-some-buffers t)))

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
(define-key global-map [remap execute-extended-command] 'smex)

(use-package which-key
  :straight t
  :init
  (which-key-mode))

;; Evil
(use-package evil
  :straight t
  :init
  (setq evil-vsplit-window-right t)
  ;; Bind it to something silly since I use C-z to run terminal
  (setq evil-toggle-key "C-c C-z")
  (setq evil-want-C-u-scroll t)
  (setq evil-split-window-below t)
  (setq evil--jumps-buffer-targets "\\(\\*\\(\\new\\|scratch\\)\\*\\|Dired:.+\\)")
  (defun vj-rename-dired-buffer ()
    "Rename dired buffer <buffer_name> to Dired:<buffer_name>."
    (interactive)
    (unless (string-match-p "Dired:" (buffer-name))
      (rename-buffer (concat "Dired:" (buffer-name)) t)))
  :hook ((dired-mode . vj-rename-dired-buffer)
         (after-init . evil-mode))
  :config
  (evil-add-command-properties #'ido-dired :jump t)
  (evil-add-command-properties #'dired-find-file :jump t)
  ;; Set normal mote for terminal-mode so we can have
  ;; block cursor. May be there is a better way
  (evil-set-initial-state 'term-mode 'normal)
  (use-package evil-surround
    :straight t
    :config (global-evil-surround-mode t))
  (with-eval-after-load 'smart-jump
    (evil-define-key 'motion prog-mode-map
      "gd" 'smart-jump-go
      "gr" 'smart-jump-references)))

;; Magit
(use-package magit
  :straight t
  :defer t
  :bind ("C-c g" . magit-status)
  :config
  (use-package evil-magit
    :straight t
    :after evil
    :config (evil-magit-init)))

(use-package gitignore-mode
  :straight t)

;; Git gutter
(use-package diff-hl
  :straight t
  :hook ((dired-mode . diff-hl-dired-mode)
         (magit-post-refresh . diff-hl-magit-post-refresh))
  :config
  (global-diff-hl-mode t)
  (diff-hl-flydiff-mode 1))

(use-package git-link
  :straight t
  :custom (git-link-open-in-browser t))

;; Search
(use-package ag
  :straight t)

;; Projectile
(use-package projectile
  :straight t
  :init
  (setq projectile-switch-project-action #'projectile-dired)
  :config
  (projectile-mode 1)
  (with-eval-after-load 'evil
    (evil-define-key 'motion global-map
      " p" 'projectile-command-map)))

;; Modeline
(straight-use-package 'minions)
(minions-mode t)

;; Shell (if I accidentally end up there)
(setq comint-prompt-read-only nil)
(setq comint-input-ignoredups t)

(defun parse-ansi-for-shell-command-output (message-or-buffer &rest _)
  "Parse ANSI escape characters in MESSAGE-OR-BUFFER.
But only if it's *Shell Command Output* buffer."
  (let ((buf message-or-buffer))
    (and (bufferp buf)
         (string= (buffer-name buf) "*Shell Command Output*")
         (with-current-buffer buf
           (ansi-color-apply-on-region (point-min) (point-max))))))
(advice-add 'display-message-or-buffer :before #'parse-ansi-for-shell-command-output)


;; ansi-term (I actually use it)
;; This section is a total mess
(setq term-input-ignoredups t)
(defun vj-term ()
  "Original term function constantly asks for my shell.
Also, I don't want to multiply terminals.
So switch to existing *ansi-term* is buffer exists"
  (interactive)
  (let ((buf (get-buffer "*ansi-term*")))
    (if (bufferp buf)
        (switch-to-buffer-other-window buf)
      (switch-to-buffer-other-window (get-buffer (buffer-name)))
      (ansi-term shell-file-name))))
(global-set-key (kbd "C-z") 'vj-term)
(advice-add 'term-char-mode :after #'evil-emacs-state)
(advice-add 'term-line-mode :after #'evil-normal-state)
(add-hook 'term-mode-hook
          (lambda ()
            (define-key term-raw-map (kbd "M-x") 'execute-extended-command)))

;; Parens
(electric-pair-mode t)

;; editorconfig
(straight-use-package 'editorconfig)
(editorconfig-mode t)

;; Markdown
(use-package markdown-mode
  :straight t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

; Go to definition
(use-package smart-jump
  :straight t
  :config
  (smart-jump-setup-default-registers))

;; Python
(defun vj-comint-clear-buffer (&optional buffer-or-name)
  "Same as plain `comint-clear-buffer' but can pass BUFFER-OR-NAME.
I often work with two splits - the code and inferior shell.
With this function I don't need to switch to comint window to clear it"
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
                             (vj-comint-clear-buffer "*Python*")))))

;; Anaconda mode
(use-package anaconda-mode
  :straight t
  :hook ((python-mode . anaconda-mode)
         (python-mode . anaconda-eldoc-mode))
  :config
  (use-package company-anaconda
    :straight t
    :after company
    :config (add-to-list 'company-backends 'company-anaconda)))

;; Virtualenvwrapper
(use-package virtualenvwrapper
  :straight t
  :defer t
  :init
  (setq venv-location "~/.cache/pypoetry/virtualenvs/")
  :config
  (venv-initialize-interactive-shells))

;; Pytest
(use-package python-pytest
  :straight t
  :custom
  (python-pytest-executable "poetry run pytest")
  (python-pytest-unsaved-buffers-behavior 'save-all))

;; Web/javascript
(use-package rjsx-mode
  :straight t)

(use-package web-mode
  :straight t
  :init
  (setq web-mode-block-padding 2
        web-mode-code-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-comment-style 2
        web-mode-css-indent-offset 2
        web-mode-enable-auto-closing t
        web-mode-enable-auto-pairing t
        web-mode-enable-comment-keywords t
        web-mode-enable-css-colorization t
        web-mode-enable-current-element-highlight t
        web-mode-markup-indent-offset 2)
  :mode (("\\.html?\\'" . web-mode)
         ("\\.tsx\\'" . web-mode)
         ("\\.jsx\\'" . web-mode)
         ("\\.js\\'" . web-mode))
  :hook ((web-mode . (lambda ()
                       (setq evil-shift-width 2)
                       (when (or (string-equal "tsx" (file-name-extension buffer-file-name))
                                 (string-equal "jsx" (file-name-extension buffer-file-name))
                                 (string-equal "js" (file-name-extension buffer-file-name)))
                         (setup-tide-mode))))
         (web-mode . rjsx-minor-mode)
         (web-mode . js2-refactor-mode)))

(use-package company-web
  :straight t
  :after company
  :hook (web-mode . (lambda ()
    (add-to-list 'company-backends 'company-web-html))))

(use-package js2-mode
  :straight t
  :init
  (setq js2-include-node-externs t)
  (setq js2-include-browser-externs t)
  :custom
  (js-switch-indent-offset 2)
  (js2-basic-offset 2)
  :config
  (js2-imenu-extras-mode))

(use-package js2-refactor
  :straight t
  :hook (js2-mode . js2-refactor-mode)
  :config
  (js2r-add-keybindings-with-prefix "C-c C-m"))

(use-package add-node-modules-path
  :straight t
  :hook (js2-mode . add-node-modules-path))

(defun setup-tide-mode ()
  "Custom Tide setup function."
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (company-mode +1))

(use-package tide
  :straight t
  :hook
  (js2-mode . setup-tide-mode))

(use-package json-mode
  :straight t)

;; Company
(use-package company
  :straight t
  :init
  (defun vj-company-abort ()
    "I'm using this when aborting company popup with ESC or C-["
    (interactive)
    (company-abort) (evil-normal-state))
  :custom
  (company-require-match nil)
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.2)
  (company-tooltip-align-annotation t)
  (company-frontends '(company-pseudo-tooltip-frontend
                       company-echo-metadata-frontend))
  :hook ((prog-mode . company-mode)
         ;; For some reason I can't use C-n/C-p which I defined for
         ;; company-active-map at the same time diff-hl is working.
         ;; So I have to disable either evil or diff-hl during
         ;; company completions.
         ;; Maybe diff-hl messes with keymaps somehow during updates?
         ;; I didn't have this problem before I've installed diff-hl.
         (company-completion-started . (lambda (_) (setq evil-emacs-state-cursor 'bar) (evil-emacs-state)))
         (company-completion-finished . (lambda (_) (setq evil-emacs-state-cursor nil) (evil-insert-state)))
         (company-completion-cancelled . (lambda (_) (setq evil-emacs-state-cursor nil) (evil-insert-state))))
  :bind (
         :map company-active-map
         ("C-n" . company-select-next)
         ("C-p" . company-select-previous)
         ("<escape>" . vj-company-abort)
         ("C-[" . vj-company-abort)
	 :map company-search-map
	 ("C-n" . company-select-next)
	 ("C-p" . company-select-previous))
  :config
  (use-package company-quickhelp
    :straight t
    :init
    (company-quickhelp-mode 1)
    (use-package pos-tip
      :straight t)))

(use-package graphql-mode
  :straight t)

;; Flycheck
(use-package flycheck
  :straight t
  :init
  (define-fringe-bitmap 'vj-flycheck-fringe-indicator
    (vector #b0000000000000000
            #b0000000000000000
            #b0000000000000000
            #b0000000000000000
            #b0000000000000000
            #b1111111111111111
            #b1111111111111111
            #b1111111111111111
            #b1111111111111111
            #b1111111111111111
            #b1111111111111111
            #b0000000000000000
            #b0000000000000000
            #b0000000000000000
            #b0000000000000000
            #b0000000000000000
            #b0000000000000000) nil 16)
  :custom (flycheck-indication-mode 'right-fringe)
  :hook (after-init . global-flycheck-mode)
  :config
  (flycheck-define-error-level 'error
    :severity 2
    :overlay-category 'flycheck-error-overlay
    :fringe-bitmap 'vj-flycheck-fringe-indicator
    :fringe-face 'flycheck-fringe-error)
  (flycheck-define-error-level 'warning
    :severity 1
    :overlay-category 'flycheck-warning-overlay
    :fringe-bitmap 'vj-flycheck-fringe-indicator
    :fringe-face 'flycheck-fringe-warning)
  (flycheck-define-error-level 'info
    :severity 0
    :overlay-category 'flycheck-info-overlay
    :fringe-bitmap 'vj-flycheck-fringe-indicator
    :fringe-face 'flycheck-fringe-info))

;; Elisp
(use-package package-lint
  :straight t)

(use-package emojify
  :straight t)

;; TRAMP
(setq tramp-backup-directory-alist backup-directory-alist)
(with-eval-after-load 'tramp-cache
  (setq tramp-persistency-file-name "~/.emacs.d/etc/tramp"))

(setq custom-file (concat (file-name-directory user-init-file) "custom-variables.el"))
(load custom-file)

(provide 'init)
;;; init.el ends here
