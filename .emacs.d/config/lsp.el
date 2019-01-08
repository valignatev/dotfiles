;;; -*- lexical-binding: t -*-
(use-package eglot
  :init (setq eglot-put-doc-in-help-buffer t)
  :commands (eglot)
  :config
  ;; Disable annoying help popup every time I put cursor on function
  (add-to-list 'eglot-ignored-server-capabilites :hoverProvider))
