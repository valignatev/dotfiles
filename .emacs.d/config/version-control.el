;;; -*- lexical-binding: t -*-

(use-package magit
  :init (setq magit-diff-refine-hunk t)
  :commands (magit-status)
  :bind (("C-x g" . magit-status)))

(use-package diff-hl
  :hook ((dired-mode . diff-hl-dired-mode)
         (magit-post-refresh . diff-hl-magit-post-refresh))
  :config
  (global-diff-hl-mode t))

(use-package git-link
  :commands (git-link git-link-commit git-link-open-in-browser)
  :custom (git-link-open-in-browser t))
