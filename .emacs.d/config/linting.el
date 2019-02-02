;;; -*- lexical-binding: t -*-

(use-package flycheck
  :init
  (define-fringe-bitmap 'flycheck-fringe-indicator
    (vector #b0000000000000000
	    #b0000000000000000
	    #b0000000000000000
	    #b0000000000000000
	    #b0000000000000000
	    #b1111111111111111
	    #b1111111111111111
	    #b1111111111111111
	    #b1111111111111111
	    #b1111111111111111
	    #b1111111111111111
	    #b0000000000000000
	    #b0000000000000000
	    #b0000000000000000
	    #b0000000000000000
	    #b0000000000000000
	    #b0000000000000000) nil 16)
  :custom (flycheck-indication-mode 'right-fringe)
  :hook (prog-mode . global-flycheck-mode)
  :config
  (flycheck-define-error-level 'error
			       :severity 2
			       :overlay-category 'flycheck-error-overlay
			       :fringe-bitmap 'flycheck-fringe-indicator
			       :fringe-face 'flycheck-fringe-error)
  (flycheck-define-error-level 'warning
			       :severity 1
			       :overlay-category 'flycheck-warning-overlay
			       :fringe-bitmap 'flycheck-fringe-indicator
			       :fringe-face 'flycheck-fringe-warning)
  (flycheck-define-error-level 'info
			       :severity 0
			       :overlay-category 'flycheck-info-overlay
			       :fringe-bitmap 'flycheck-fringe-indicator
			       :fringe-face 'flycheck-fringe-info))

(use-package package-lint
  :commands (package-lint-current-buffer))
