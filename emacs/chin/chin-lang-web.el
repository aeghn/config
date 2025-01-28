;;; -*- lexical-binding: t; -*-

(use-package typescript-ts-mode
  :hook (typescript-ts-mode . eglot-ensure)
  :hook (typescript-ts-mode . electric-pair-mode)
  :hook (tsx-ts-mode . eglot-ensure)
  :hook (tsx-ts-mode . electric-pair-mode)
  :init
  (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))
  )


(provide 'chin-lang-web)
