;;; -*- lexical-binding: t -*-

(use-package docker
  :bind ("C-c d" . docker))

(use-package dockerfile-mode
  :mode "Dockerfile\\'")
