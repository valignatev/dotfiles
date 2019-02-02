;;; -*- lexical-binding: t -*-

(defun web-mode-defaults ()
  (setq web-mode-block-padding 2
        web-mode-code-indent-offset 2
        web-mode-comment-style 2
        web-mode-css-indent-offset 2
        web-mode-enable-auto-closing t
        web-mode-auto-close-style 2
        web-mode-enable-auto-pairing t
        web-mode-enable-comment-keywords t
        web-mode-enable-html-entities-fontification t
        web-mode-enable-css-colorization t
        web-mode-enable-current-element-highlight t
        web-mode-markup-indent-offset 2))

(use-package web-mode
  :straight t
  :init (web-mode-defaults)
  :mode (("\\.html?\\'" . web-mode )))

(use-package emmet-mode
  :defer t)
