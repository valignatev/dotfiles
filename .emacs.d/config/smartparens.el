;;; -*- lexical-binding: t -*-

(setq sp-highlight-pair-overlay nil
      sp-cancel-autoskip-on-backward-movement nil
      sp-show-pair-delay 0
      sp-show-pair-from-inside t
      ;; TODO: doesn't work,
      ;; track https://github.com/Fuco1/smartparens/issues/961
      sp-escape-quotes-after-insert nil
      sp-escape-wrapped-region nil)

;; Stolen from http://web-mode.org/ "I want to use smartparens" section
;; and modified with eval-after-load and use-package
(defun vj/sp-web-mode-hook ()
  (setq web-mode-enable-auto-pairing nil))

(eval-after-load 'web-mode
  (add-hook 'web-mode-hook  'vj/sp-web-mode-hook))

(defun sp-web-mode-is-code-context (id action context)
  (and (eq action 'insert)
       (not (or (get-text-property (point) 'part-side)
                (get-text-property (point) 'block-side)))))

(use-package smartparens
  :hook (after-init . smartparens-global-mode)
  :config
  (require 'smartparens-config)
  (sp-local-pair 'web-mode "<" nil :when '(sp-web-mode-is-code-context)))

;; TODO: this is ugly as hell, do something with it
(defun my-create-newline-and-enter-sexp (&rest _ignored)
  "Open a new brace or bracket expression, with relevant newlines and indent."
  (newline)
  (indent-according-to-mode)
  (forward-line -1)
  (indent-according-to-mode))

(with-eval-after-load 'smartparens
  (sp-local-pair 'c-mode "{" nil :post-handlers
                 '((my-create-newline-and-enter-sexp "RET")))
  (sp-local-pair 'java-mode "{" nil :post-handlers
                 '((my-create-newline-and-enter-sexp "RET")))
  (sp-local-pair 'python-mode "[" nil :post-handlers
                 '((my-create-newline-and-enter-sexp "RET")))
  (sp-local-pair 'python-mode "(" nil :post-handlers
                 '((my-create-newline-and-enter-sexp "RET"))))
