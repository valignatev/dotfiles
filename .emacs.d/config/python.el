;;; -*- lexical-binding: t -*-

(setq python-indent-guess-indent-offset-verbose nil
      python-shell-interpreter "python")

;; Figure out why it doesn't work in loops
;; (when (executable-find "ipython")
;;   (setq python-shell-interpreter "ipython"
;;         python-shell-interpreter-args "-i --simple-prompt --no-color-info"
;;         python-shell-prompt-regexp "In \\[[0-9]+\\]: "
;;         python-shell-prompt-block-regexp "\\.\\.\\.\\.: "
;;         python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
;;         python-shell-completion-setup-code
;;         "from IPython.core.completerlib import module_completion"
;;         python-shell-completion-string-code
;;         "';'.join(get_ipython().Completer.all_completions('''%s'''))\n"))
