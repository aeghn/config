;;; -*- lexical-binding: t; -*-

(use-package rust-ts-mode
  :hook (rust-ts-mode . eglot-ensure)
  :hook (rust-ts-mode . subword-mode)
  :hook (rust-ts-mode . electric-pair-mode)
  :hook (rust-ts-mode . cargo-minor-mode)
  :init
  (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-ts-mode))
  :config
  (message "add to list rs")
  )

(provide 'chin-lang-rust)
