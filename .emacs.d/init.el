;;; init.el -- My emacs configuration
;-*-Emacs-Lisp-*-
;;; Commentary:
;; Why linter wants me to do this?
;;
;;; Code:

;; My custom-file.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;; Essential settings.
(setq inhibit-startup-message t
      inhibit-splash-screen t
      inhibit-startup-echo-area-message t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(when (boundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(show-paren-mode 1)
(setq column-number-mode t)
(blink-cursor-mode 0)
(desktop-save-mode 1)
(global-visual-line-mode 1)

;; Default font
;; (add-to-list 'default-frame-alist
;;              '(font . "FuraCode Nerd Font Mono-10"))

;; Store backups and autosaves in temp directory
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; When point goes outside the window, Emacs recenters the buffer point.
;; This changes scrolling behaviour to only as far as point goes
(setq scroll-conservatively 101)

;; Highlight the current line
(when window-system
  (global-hl-line-mode))

;; No tabs
(setq-default indent-tabs-mode nil)
;; But I won't mess current file indentation
(defun infer-indentation-style ()
  "Figuring out current indentation type and stick with it."
  (let ((space-count (how-many "^ " (point-min) (point-max)))
        (tab-count (how-many "^\t" (point-min) (point-max))))
    (if (> space-count tab-count) (setq indent-tabs-mode nil))
    (if (> tab-count space-count) (setq indent-tabs-mode t))))


;; Shorten yes or no.
(defalias 'yes-or-no-p 'y-or-n-p)

;; Delete trailing whitespaces before saving
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(setq require-final-newline t)
;; When something changes a file, automatically refresh the buffer containing that file.
(global-auto-revert-mode t)

;; Package stuff
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
(package-install-selected-packages)

;; Evil
(use-package evil
  :ensure t
  :init
  (setq evil-want-C-u-scroll t)
  :config
  (evil-mode 1)
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)
  (modify-syntax-entry ?_ "w")

  (use-package evil-leader
    :ensure t
    :config
    (global-evil-leader-mode)
    (evil-leader/set-leader "<SPC>")
    (evil-leader/set-key
      "<SPC>" 'counsel-M-x)
    )

  (use-package evil-surround
    :ensure t
    :config
    (global-evil-surround-mode)))


;; Use Evil motions in occur-mode
(add-hook 'occur-mode-hook
          (lambda ()
            (evil-add-hjkl-bindings occur-mode-map 'emacs
              (kbd "/") 'evil-search-forward
              (kbd "?") 'evil-search-backward
              (kbd "n") 'evil-search-next
              (kbd "N") 'evil-search-previous
              (kbd "C-d") 'evil-scroll-down
              (kbd "C-u") 'evil-scroll-up
              (kbd "C-w C-w") 'other-window)))

;; Sorting engine. Used by swiper and counsel
(use-package flx
  :ensure t)

;; Ivy
(use-package ivy
  :ensure t
  :config
  (setq ivy-use-virtual-buffers t
        ivy-count-format "%d/%d"
        ivy-re-builders-alist '((t . ivy--regex-fuzzy)))
  (ivy-mode )

  (use-package swiper
    :ensure t)

    (use-package counsel
        :ensure t
        :config
        (global-set-key (kbd "M-x") 'counsel-M-x)
        (global-set-key (kbd "C-x C-f") 'counsel-find-file))
    )

;; PATH from shell
(use-package exec-path-from-shell
  :disabled t
  :if (memq window-system '(mac ns))
  :config
  (setq exec-path-from-shell-check-startup-files nil
        exec-path-from-shell-variables '("PATH"))
  (exec-path-from-shell-initialize))

;; Org-mode
(use-package org-bullets
  :ensure t
  :config (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(add-hook 'org-mode-hook (lambda () (setq org-src-fontify-natively t)))
(add-hook 'org-mode-hook (lambda ()
                           (org-babel-do-load-languages
                            'org-babel-load-languages
                            '((python . t)))
                           ))

;; Flycheck
(use-package flycheck
  :ensure t
  :config
  (add-hook 'after-init-hook 'global-flycheck-mode)

  (add-hook 'flycheck-mode-hook
            (lambda ()
              (evil-define-key
                'normal flycheck-mode-map (kbd "]e")
                'flycheck-next-error)
              (evil-define-key
                'normal flycheck-mode-map (kbd "[e")
                'flycheck-previous-error))
            ))

;; Markdown
(use-package markdown-mode
  :ensure t)

;; Web-mode stuff
;; File type overrides for web-mode
(add-to-list 'auto-mode-alist '("\\.html$" . web-mode))

(use-package web-mode
  :ensure t
  :defer t
  :init
  (setq web-mode-engines-alist
        '(("django" . "\\.html\\'")))
  (setq web-mode-enable-auto-quoting t)
  :config
  (setq web-mode-attr-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-indent-style 2
        web-mode-markup-indent-offset 2
        web-mode-sql-indent-offset 2)

  (add-hook 'web-mode-hook
            (lambda ()
              (setq web-mode-style-padding 2)
              (flycheck-mode))))

;; Javascript
(use-package js2-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
  (add-to-list 'interpreter-mode-alist '("node" . js2-mode))
  (setq js2-basic-offset 2)

  (use-package rjsx-mode
    :ensure t))

;; Company
(use-package company
  :ensure t
  :init (global-company-mode)
  :config
  (setq company-minimum-prefix-length 1)
  (setq company-idle-delay 0.2)

  (use-package company-tern
    :ensure t
    :config
    (add-to-list 'company-backends 'company-tern)))

;; Python stuff
(use-package anaconda-mode
  :ensure t
  :config
  (add-hook 'python-mode-hook 'anaconda-mode)
  (add-hook 'python-mode-hook 'anaconda-eldoc-mode)
  (add-to-list 'company-backends #'company-anaconda)

  (use-package company-anaconda
    :ensure t
    :config
    (eval-after-load "company"
      '(add-to-list 'company-backends 'company-anaconda))
    )
  )

(use-package virtualenvwrapper
  :ensure t
  :config
  (venv-initialize-interactive-shells)
  (venv-initialize-eshell)
  (setq venv-location "~/.virtualenvs/"))

; Smartparens
(use-package smartparens-config
  :ensure smartparens
  :init
  (add-hook 'prog-mode-hook #'smartparens-mode)

  (use-package evil-smartparens
    :ensure t
    :init
    (add-hook 'smartparens-enabled-hook #'evil-smartparens-mode)))

;; Projectile
(use-package projectile
  :ensure t
  :config
  (projectile-mode)
  (setq projectile-completion-system 'ivy)
  ;; workaround for https://github.com/bbatsov/projectile/issues/1183
  (setq projectile-mode-line
        '(:eval (format " Projectile[%s]"
                        (projectile-project-name)))))

;; YAML
(use-package yaml-mode
  :ensure t
  :config
  (add-hook 'yaml-mode-hook
        (lambda ()
            (define-key yaml-mode-map "\C-m" 'newline-and-indent))))

(provide 'init)
;;; init.el ends here
