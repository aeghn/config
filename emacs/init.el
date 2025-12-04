;;; -*- lexical-binding: t; -*-

;;; Custom file Settings
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

(setq evil-want-keybinding nil)

;;; Loading Tools
;;; Platform Settings
(defconst chin/is-linux   (eq system-type 'gnu/linux))
(defconst chin/is-windows (memq system-type '(cygwin windows-nt ms-dos)))
(defconst chin/is-android (string-match-p "-linux-android$" system-configuration))

(setq-default tsinghua-mirror
              '(("gnu"   . "https://mirrors.bfsu.edu.cn/elpa/gnu/")
                ("melpa" . "https://mirrors.bfsu.edu.cn/elpa/melpa/")))



(setq package-archives tsinghua-mirror)
;; (package-quickstart-refresh )

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "chin" user-emacs-directory))

(when chin/is-windows
  ;; TODO remove this
  (setq package-check-signature nil))

;; Locale Settings
(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))
(prefer-coding-system 'utf-8)
(setq system-time-locale "C")

(use-package savehist
  :config
  (savehist-mode))


;; Avoid the ask, just visit the direct file.
(setq vc-follow-symlinks nil)

(defun chin/server-shutdown ()
  "Save buffers, quit, and shutdown (kill) server."
  (interactive)
  (save-some-buffers)
  (kill-emacs))

(require 'chin-ui)
(require 'chin-completion)
(require 'chin-edit)
;; (require 'chin-evil)
(require 'chin-file)
(require 'chin-window-and-buffer)
(require 'chin-lang-web)
(require 'chin-chinese)
(require 'chin-lsp)
(require 'chin-org)
(require 'chin-project)
(require 'chin-vc)
(require 'chin-lang-rust)
(require 'chin-write)
