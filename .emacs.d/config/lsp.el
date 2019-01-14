;;; -*- lexical-binding: t -*-
(use-package eglot
  :init (setq eglot-put-doc-in-help-buffer t)
  :commands (eglot)
  :config
  ;; Disable annoying help popup every time I put cursor on function
  (add-to-list 'eglot-ignored-server-capabilites :hoverProvider))

(use-package lsp-mode
  :defer t)
(use-package lsp-ui
  :after lsp-mode
  :init (setq lsp-ui-peek-always-show t
              lsp-ui-sideline-enable nil
              lsp-ui-doc-enable nil)
  :config
  (define-key lsp-ui-mode-map
    [remap xref-find-references]
    #'lsp-ui-peek-find-references))

(use-package company-lsp
  :commands (company-lsp))
