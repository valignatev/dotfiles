;;; -*- lexical-binding: t -*-

(use-package projectile
  :defer 5
  :init
  (setq projectile-switch-project-action #'projectile-dired
        projectile-completion-system 'ivy
        ;; this is a workaround, check https://github.com/bbatsov/projectile/issues/1323#issuecomment-469179960
        projectile-git-submodule-command nil)
  :bind-keymap ("C-c p" . projectile-command-map)
  :config (projectile-global-mode))

(use-package counsel-projectile
  :after (counsel projectile)
  :config
  (counsel-projectile-mode 1))
