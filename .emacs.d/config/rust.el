;;; -*- lexical-binding: t -*-

(use-package rust-mode
  :straight t
  :mode (("\\.rs\\'" . rust-mode)))

(use-package flycheck-rust
  :straight t
  :after rust-mode
  :hook (flycheck-mode . flycheck-rust-setup))
