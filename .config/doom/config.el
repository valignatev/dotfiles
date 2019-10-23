;;; ~/.config/doom/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here
(fringe-mode 16)

(setq doom-font (font-spec :family "Hack" :size 24)
      doom-theme 'spacemacs-light
      spacemacs-theme-comment-bg nil
      yas-indent-line 'fixed
      lsp-ui-sideline-enable nil
      org-startup-truncated nil
      flycheck-display-errors-delay 0.5
      display-line-numbers-type nil
      evil-echo-state nil)

;; Make doom-modeline leaner, see
;; https://github.com/seagle0128/doom-modeline/issues/187#issuecomment-507201556
(defun my-doom-modeline--font-height ()
  "Calculate the actual char height of the mode-line."
  (+ (frame-char-height) 2))
(advice-add #'doom-modeline--font-height :override #'my-doom-modeline--font-height)
(add-to-list '+doom-solaire-themes '(spacemacs-light . t))

(after! lsp-ui
  (setq lsp-eldoc-enable-hover t))

(after! git-commit
  (setq git-commit-setup-hook (delete
                               'git-commit-turn-on-auto-fill
                               git-commit-setup-hook))
  (setq git-commit-summary-max-length 72
        fill-column nil))

(after! (lsp-rust)
  (setq global-mode-string
        (delete (list '(t lsp-clients-rust-progress-string)) global-mode-string)))
(map!
 :i "C-w" 'evil-window-map)


(defconst python-f-string-regexp
  "{\\s-*\\(\\sw\\|\\s_\\)+\\.?\\(\\sw\\|\\s_\\)+\\s-*}"
  "Spaces following by underscores or words following by optional dot and
words/underscores again.")

(defun python-f-string-font-lock-find (limit)
  (while (re-search-forward python-f-string-regexp limit t)
      (when (python-syntax-comment-or-string-p)
        (put-text-property (match-beginning 0) (match-end 0)
                           'face 'font-lock-variable-name-face)))
  nil)

(after! python
  (setq python-indent-def-block-scale 1)
  (progn (font-lock-add-keywords
          'python-mode
          `((python-f-string-font-lock-find))
          'append)))

(use-package! glsl-mode
  :mode "\\.glsl\\'"
  :mode "\\.vert\\'"
  :mode "\\.frag\\'"
  :mode "\\.geom\\'")
