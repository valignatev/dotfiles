;; -*- lexical-binding: t; -*-
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message user-login-name
      inhibit-default-init t
      create-lockfiles nil
      ;; Starting scratch buffer in fundamental mode instead
      ;; of elisp-mode saves startup time
      initial-major-mode 'fundamental-mode
      initial-scratch-message nil
      scroll-conservatively 101
      x-gtk-use-system-tooltips nil
      use-dialog-box nil
      auto-window-vscroll nil
      vc-follow-symlinks t
      confirm-kill-processes nil
      echo-keystrokes 0.5
      dired-dwim-target t
      tab-always-indent t
      ;; 1mb
      read-process-output-max (* 1024 1024)
      column-number-indicator-zero-based nil
      save-interprogram-paste-before-kill t
      truncate-partial-width-windows nil
      ring-bell-function 'ignore)
(setq-default indent-tabs-mode nil
              truncate-lines t)

(column-number-mode 1)
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(blink-cursor-mode 0)
(set-frame-font "Hack-12" t t)
(show-paren-mode t)
(global-auto-revert-mode t)
(savehist-mode 1)
(recentf-mode 1)
(winner-mode 1)
(defalias 'yes-or-no-p 'y-or-n-p)
(advice-add #'display-startup-echo-area-message :override #'ignore)

(defvar IS-WINDOWS (eq system-type 'windows-nt))
(when IS-WINDOWS
  (setq w32-get-true-file-attributes nil
        w32-pipe-read-delay 0
        w32-pipe-buffer-size (* 64 1024)))

(defun my/edit-init-file ()
  "Opens init.el for editing"
  (interactive)
  (find-file user-init-file))

(defun my/terminal-in-project-root (arg)
  "Opens the $TERMINAL in project root.
With ARG, opens in in the current working directory"
  (interactive "P")
  (let ((default-directory
          (if arg default-directory
            (cdr (project-current)))))
    (start-process "terminal" nil (getenv "TERMINAL"))))

;; straight.el boilerplate
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

(setq use-package-hook-name-suffix nil)
(straight-use-package 'use-package)

(use-package spacemacs-theme
  :defer t
  :init
  (setq spacemacs-theme-comment-bg nil
        spacemacs-theme-comment-italics t)
  (load-theme 'spacemacs-light t)
  :custom-face
  (font-lock-type-face ((t (:inherit nil))))
  :hook
  ((dired-mode-hook . dired-hide-details-mode)))

(use-package emacs
  :mode (("Pipfile\\'" . conf-toml-mode)
         ("Pipfile.lock\\'" . js-mode)
         ("requirements.txt\\'" . conf-mode)
         ("\\.styl\\'" . css-mode))
  :bind (("C-x t" . my/terminal-in-project-root))
  :config
  (setq js-indent-level 2
        css-indent-offset 2))

;; Magic garbage collector hack
;; It's kinda small so maybe makes sense to just copy/paste
;; it into a config instead of installing it with straight
(use-package gcmh
  :init
  (setq gcmh-idle-delay 5
        gcmh-high-cons-threshold (* 100 1024 1024))  ; 100mb
  :hook ((window-setup-hook . gcmh-mode)))

(use-package no-littering
  :config
  (require 'no-littering)
  (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))

(use-package magit
  :init
  (setq magit-diff-refine-hunk t
        git-commit-summary-max-length 73
        magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1
        magit-save-repository-buffers nil)
  :commands (magit-status)
  :bind (("C-x g" . magit-status)))

(use-package git-link
  :init
  (setq git-link-open-in-browser t)
  :commands (git-link git-link-commit)
  :custom (git-link-open-in-browser t))

(use-package evil
  :init
  (setq evil-default-state 'emacs
        evil-want-C-w-in-emacs-state t
        evil-want-C-w-delete nil
        evil-want-Y-yank-to-eol t
        evil-want-C-u-scroll t
        evil-vsplit-window-right t
        evil-split-window-below t)
  :config
  (evil-set-initial-state 'prog-mode 'normal)
  (evil-set-initial-state 'text-mode 'normal)
  (evil-set-initial-state 'conf-mode 'normal)
  (evil-set-initial-state 'fundamental-mode 'normal)
  (evil-set-initial-state 'git-commit-mode 'emacs)
  :hook (after-init-hook . evil-mode))

(use-package tree-sitter
  :config
  (require 'tree-sitter-langs)
  (global-tree-sitter-mode)
  :hook((tree-sitter-after-on-hook . tree-sitter-hl-mode)))

(use-package tree-sitter-langs
  :custom-face
  (tree-sitter-hl-face:property ((t (:inherit 'font-lock-constant-face)))))

(use-package selectrum
  :config
  (selectrum-mode +1))

(use-package prescient
  :config
  (use-package selectrum-prescient
    :after (selectrum)
    :config
    (selectrum-prescient-mode +1)
    (prescient-persist-mode +1)))

(use-package deadgrep
  :bind ("<f5>" . deadgrep))

(use-package dockerfile-mode
  :defer t)

(use-package yaml-mode
  :defer t)

(use-package zig-mode
  :defer t)

(use-package markdown-mode
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package php-mode
  :defer t)

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l"
        lsp-enable-symbol-highlighting nil)
  :hook (
         (php-mode-hook . lsp)
         (zig-mode-hook . lsp))
  :commands lsp)

(use-package web-mode
  :defer t)
