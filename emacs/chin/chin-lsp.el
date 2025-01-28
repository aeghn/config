;;; -*- lexical-binding: t; -*-

;; Eglot
(use-package eglot
  :config
  (add-hook 'rust-mode-hook 'eglot-ensure)
  (add-hook 'web-mode-hook 'eglot-ensure)
  (add-hook 'go-mode-hook 'eglot-ensure)
  (custom-set-variables
   '(help-at-pt-timer-delay 0.1)
   '(help-at-pt-display-when-idle '(flymake-diagnostic)))
  (add-hook 'eglot-managed-mode-hook
            (lambda ()
              ;; Show flymake diagnostics first.
              (setq eldoc-documentation-functions
                    (cons #'flymake-eldoc-function
                          (remove #'flymake-eldoc-function eldoc-documentation-functions)))
              ;; Show all eldoc feedback.
              (setq eldoc-documentation-strategy #'eldoc-documentation-compose)))
  :bind
  ("M-RET" . 'eglot-code-actions))

(provide 'chin-lsp)
