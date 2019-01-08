;;; -*- lexical-binding: t -*-

(use-package company
  :custom
  (company-require-match nil)
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.2)
  (company-tooltip-align-annotation t)
  (company-frontends '(company-pseudo-tooltip-frontend
		               company-echo-metadata-frontend))
  :commands (company-mode global-company-mode company-complete
                          company-complete-common company-manual-begin
                          company-grab-line)
  :bind (:map company-active-map
        "C-n" . company-select-next)
        "C-p" . company-select-previous))
  :config
  (global-company-mode +1))

(use-package company-quickhelp
  :after company
  :commands (company-quickhelp-mode)
  :init
  (company-quickhelp-mode 1)
  (use-package pos-tip
    :commands (pos-tip-show)))
