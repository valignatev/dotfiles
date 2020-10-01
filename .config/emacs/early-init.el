;; -*- lexical-binding: t; -*-

(setq gc-cons-threshold most-positive-fixnum)

;; Disable package.el
(setq package-enable-at-startup nil)
(advice-add #'package--ensure-init-file :override #'ignore)

(setq frame-inhibit-implied-resize t)
