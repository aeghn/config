;;; -*- lexical-binding: t; -*-

(require 'subr-x)

(load-theme 'modus-operandi)


;; Set tab width
(setq-default tab-width 4)
;; use space to indent by default
(setq-default indent-tabs-mode nil)

;; Frame settings
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)

(setq use-dialog-box nil
      use-short-answers t)

;; Scrolling Settings
(setq scroll-step           1
      scroll-conservatively 10000)
(pixel-scroll-precision-mode 1)
(setq pixel-scroll-precision-interpolate-page t)
(defalias 'scroll-up-command 'pixel-scroll-interpolate-down)
(defalias 'scroll-down-command 'pixel-scroll-interpolate-up)

;; Disable the annoying bell.
(setq ring-bell-function 'ignore)

;; Toolbar Settings
(tool-bar-mode -1)

(setq backup-directory-alist `(("." . "~/.emacs-saves")))

;; Paren Settings
(show-paren-mode)
(setq show-paren-style 'mixed
      show-paren-content-when-offscreen 'overlay)

(setq word-wrap-by-category t)

(use-package git-gutter
  :config
  (global-git-gutter-mode +1))

(setq-default chin/home (getenv "HOME"))

;;; Packages
(defun abbreviate-file-path (file-path)
  "Abbreviate the directory part of a file path, showing only the first letter of each directory."
  (let* ((trimmed (string-replace chin/home "~" file-path))
         (path-segments (split-string trimmed "/")))
    (mapconcat
     (lambda (segment)
       (if (length> segment 1)
           (if (string-prefix-p "." segment)
               (substring segment 0 2)
             (substring segment 0 1))
         segment))
     path-segments "/")))


(setq-default mode-line-format
                '("["
                  (:eval (cond
                          (( eq evil-state 'visual) "V")
                          (( eq evil-state 'normal) "N")
                          (( eq evil-state 'insert) "I")
                          (t "?")))
                  (:eval (when (buffer-modified-p)  " *"))
                  "] "
                  (:eval (abbreviate-file-path default-directory))
                  "%b %l,%cC %p"
                  mode-line-format-right-align
                  " "
                  (flymake-mode
                   (flymake-mode-line-title
                    flymake-mode-line-exception
                    flymake-mode-line-counters))
                  mode-line-misc-info))

(provide 'chin-ui)
