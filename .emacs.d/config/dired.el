;;; -*- lexical-binding: t -*-

(setq dired-dwim-target t)

(use-package all-the-icons-dired
  :after all-the-icons
  :hook (dired-mode . all-the-icons-dired-mode))
