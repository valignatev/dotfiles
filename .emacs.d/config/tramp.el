;;; -*- lexical-binding: t -*-

;; Store tramp backups locally
(setq tramp-backup-directory-alist backup-directory-alist)

(with-eval-after-load 'tramp-cache
  (setq tramp-persistency-file-name "~/.emacs.d/tramp"))
