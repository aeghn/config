;;; -*- lexical-binding: t; -*-

(use-package evil
  :ensure t
  :demand t
  :init
  (setq evil-respect-visual-line-mode t)
  (setq evil-want-abbrev-expand-on-insert-exit t)
  (setq evil-want-keybinding nil)
  (setq evil-undo-system 'undo-redo)
  (setq evil-disable-insert-state-bindings t)
  :bind (
         :map evil-normal-state-map
         (("C-e" . end-of-line)
          ("C-r" . isearch-backward)
          ("U" . evil-redo)
          ("M-." . xref-find-definitions))
         :map evil-visual-state-map
         (("DEL" . delete-region)))
  :config
  (evil-mode t)
  (setq evil-mode-line-format '(before . mode-line-front-space)))


(use-package evil-collection
  :ensure t
  :after evil
  :demand t
  :config
  (evil-collection-init)
  (which-key-mode)
  (evil-collection-which-key-setup))

(use-package evil-surround
  :ensure t
  :after evil
  :demand t
  :config
  (global-evil-surround-mode 1))

;; (use-package evil-org
;;   :after (evil org)
;;   :demand t
;;   :config
;;   (require 'evil-org-agenda)
;;   (evil-org-agenda-set-keys))


;; Leader key
(use-package evil-leader
  :demand t
  :ensure t
  :init
  (setq evil-want-keybinding nil)
  (setq evil-leader/in-all-states 1)
  (global-evil-leader-mode)
  :config
  (evil-leader/set-leader "<SPC>")
  (evil-leader/set-key
   "e" 'find-file
   "b" 'consult-buffer
   "k" 'kill-buffer
   "f" 'affe-find
   "d" (lambda () (interactive) (consult-flymake t))
   "a" 'eglot-code-actions
   "wh" 'windmove-left
   "wj" 'windmove-down
   "wk" 'windmove-up
   "wl" 'windmove-right
   "wx" 'delete-window
   "/" 'consult-ripgrep
   "l" 'consult-line
   ))

(provide 'chin-evil)
