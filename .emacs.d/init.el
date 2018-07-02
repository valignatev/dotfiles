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
(setq indent-tabs-mode nil)
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
  :hook (dired-mode . vj-rename-dired-buffer)
  :config
  (evil-mode 1)
  (evil-add-command-properties #'ido-dired :jump t)
  (evil-add-command-properties #'dired-find-file :jump t)
  ;; Set normal mote for terminal-mode so we can have
  ;; block cursor. May be there is a better way
  (evil-set-initial-state 'term-mode 'normal)
  (use-package evil-surround
    :straight t
    :config (global-evil-surround-mode t))
  (use-package evil-ediff
    :straight t
    :config (evil-ediff-init)))

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

;; Git gutter
(use-package diff-hl
  :straight t
  :hook ((dired-mode . diff-hl-dired-mode)
         (magit-post-refresh . diff-hl-magit-post-refresh))
  :config
  (global-diff-hl-mode t)
  (diff-hl-flydiff-mode 1))

;; Search
(use-package ag
  :straight t)

;; Projectile
(straight-use-package 'projectile)
(projectile-mode 1)

;; Modeline
(straight-use-package 'minions)
(minions-mode t)

;; Shell (if I accidentally end up there)
(setq comint-prompt-read-only t)
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
;; I need to call for disabling evil-mode for both
;; vj-term and term-char-mode because in first scenario
;; we should wait until we in *terminal* buffer before
;; disabling evil
(advice-add 'vj-term :after #'turn-off-evil-mode)
(advice-add 'term-char-mode :after #'turn-off-evil-mode)
(advice-add 'term-line-mode :after #'turn-on-evil-mode)
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
    :requires company
    :config (add-to-list 'company-backends 'company-anaconda)))

;; Virtualenvwrapper
(use-package virtualenvwrapper
  :straight t
  :defer t
  :init
  (setq venv-location "~/.cache/pypoetry/virtualenvs/")
  :config
  (venv-initialize-interactive-shells))



;; Company
(use-package company
  :straight t
  :custom
  (company-require-match nil)
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.2)
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
         (company-completion-cancelled . (lambda (_) (setq evil-emacs-state-cursor nil) (evil-normal-state))))
  :bind (
         :map company-active-map
         ("C-n" . company-select-next)
         ("C-p" . company-select-previous)
         ("<escape>" . company-abort)
	 ("C-[" . company-abort)
	 :map company-search-map
	 ("C-n" . company-select-next)
	 ("C-p" . company-select-previous)))

;; Flycheck
(use-package flycheck
  :straight t
  :hook (after-init . global-flycheck-mode))

;; Elisp
(use-package package-lint
  :straight t)

(setq custom-file (concat (file-name-directory user-init-file) "custom-variables.el"))
(load custom-file)

(provide 'init)
;;; init.el ends here
