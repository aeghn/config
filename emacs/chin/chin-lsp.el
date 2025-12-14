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
  (add-to-list 'eglot-stay-out-of 'eldoc)
  :bind
  ("M-RET" . 'eglot-code-actions))

(use-package eldoc
  :config
  (setq
   eldoc-echo-area-prefer-doc-buffer t
   eldoc-echo-area-use-multiline-p nil) )

(use-package flymake
  :config
  (setq flymake-show-diagnostics-at-end-of-line nil))

(use-package eglot-booster
  :vc (:url "https://github.com/jdtsmith/eglot-booster")
  :after eglot
  :config   (eglot-booster-mode))

(provide 'chin-lsp)
