;;; ~/.config/doom/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here
(fringe-mode 16)
(fast-scroll-config)
(fast-scroll-mode 1)
(minions-mode)
(electric-pair-mode 1)
(modify-syntax-entry ?_ "w" (standard-syntax-table))

(after! smartparens
  (smartparens-global-mode -1))

(setq doom-font (font-spec :family "Hack" :size 24)
      ;; Live long and suffer 🖖
      doom-unicode-font "Noto Color Emoji:pixelsize=25:weight=normal"
      doom-theme 'spacemacs-light
      doom-gc-cons-threshold 36777216
      spacemacs-theme-comment-bg nil
      yas-indent-line 'fixed
      lsp-ui-sideline-enable nil
      lsp-enable-snippet nil
      lsp-rust-server 'rust-analyzer
      org-startup-truncated nil
      flycheck-display-errors-delay 0.5
      display-line-numbers-type nil
      evil-echo-state nil
      evil-split-window-below t
      evil-vsplit-window-right t
      +evil-want-o/O-to-continue-comments nil
      company-box-doc-enable nil)

(add-to-list '+doom-solaire-themes '(spacemacs-light . t))
(after! solaire-mode
  (setq hl-line-range-function nil))

(after! lsp-ui
  (setq lsp-eldoc-enable-hover t
        lsp-enable-symbol-highlighting nil))

(after! git-commit
  (setq git-commit-setup-hook (delete
                               'git-commit-turn-on-auto-fill
                               git-commit-setup-hook)
        git-commit-summary-max-length 72
        fill-column nil))

(after! (lsp-rust)
  (setq global-mode-string
        (delete (list '(t lsp-clients-rust-progress-string)) global-mode-string)))

(map!
 :i "C-w" 'evil-window-map)

(add-hook '+popup-buffer-mode-hook
  (lambda () (visual-line-mode 1)))

(use-package! glsl-mode
  :mode "\\.glsl\\'"
  :mode "\\.vert\\'"
  :mode "\\.frag\\'"
  :mode "\\.geom\\'")
