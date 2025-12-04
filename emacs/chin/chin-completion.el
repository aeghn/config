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
  
  (global-set-key (kbd "M-s l") 'consult-line)
  )

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
  (global-set-key (kbd "M-s f") 'affe-find)
  )

(use-package consult-xref
  :defer t
  :init
  (setq xref-show-xrefs-function       #'consult-xref
        xref-show-definitions-function #'consult-xref))


(use-package embark
  :bind
  (("C-," . embark-act)
   ("C-." . embark-dwim)
   ("C-h B" . embark-bindings)
   :map minibuffer-mode-map
   ("C-c C-o" . embark-export)  ;; This is the default binding of Ivy-Occur
   )
  :config
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :after (embark consult)
  :hook
  (embark-consult-mode . consult-preview-at-point-mode))

(provide 'chin-completion)
