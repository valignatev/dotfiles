;;; -*- lexical-binding: t -*-

(defun vj/terminal-in-project-root (arg)
  (interactive "P")
  (let ((default-directory
	    (if arg default-directory
	      (projectile-project-root))))
    (start-process "terminal" nil (getenv "TERMINAL"))))

(global-set-key (kbd "C-x t") 'vj/terminal-in-project-root)
