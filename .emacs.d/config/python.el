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

(use-package virtualenvwrapper
  :straight t
  :defer t
  ;; all dirs from pipenv and poetry-based virtualenv dirs
  :init
  (setq venv-location
        (apply
         'append (mapcar
                  (lambda (loc)
                    (directory-files loc t directory-files-no-dot-files-regexp))
                  '("~/.cache/pypoetry/virtualenvs/"
			        "~/.local/share/virtualenvs/"))))
  :config
  (venv-initialize-interactive-shells))

(use-package python-pytest
  :straight t
  :defer t
  :custom
  (python-pytest-executable "poetry run pytest")
  (python-pytest-unsaved-buffers-behavior 'save-all))

(use-package anaconda-mode
  :disabled t
  :hook ((python-mode . anaconda-mode)
         (python-mode . anaconda-eldoc-mode))
  :config
  (use-package company-anaconda
    :after company
    :config (add-to-list 'company-backends 'company-anaconda)))

(use-package ms-python
  :after lsp-mode
  :straight (:host github :repo "xhcoding/ms-python")
  :config
  (add-hook 'python-mode-hook #'lsp))
