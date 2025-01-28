;;; -*- lexical-binding: t; -*-

(use-package vertico
  :config
  (vertico-mode))

(use-package consult
  :config
  (setq read-file-name-completion-ignore-case t
        read-buffer-completion-ignore-case t
        completion-ignore-case t
        completion-category-defaults nil
        completion-category-overrides nil
        completion-styles '(basic substring partial-completion flex))

  (setq completion-in-region-function #'consult-completion-in-region)
  (consult-customize consult-completion-in-region
                     :completion-styles '(basic)
                     :cycle-threshold 3)

  ;; Consult Settings
  (global-set-key (kbd "M-3") 'consult-ripgrep)
  (global-set-key (kbd "M-4") 'consult-buffer)
  (global-set-key (kbd "M-s l") 'consult-line))

(use-package corfu
  :config
  (setq corfu-auto t)
  (setq corfu-quit-at-boundary t)

  (global-corfu-mode)
  (setq text-mode-ispell-word-completion nil))

(use-package affe
  :config
  ;; Manual preview key for `affe-grep'
  (consult-customize affe-grep :preview-key "M-.")
  )

(provide 'chin-completion)
