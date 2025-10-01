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
(scroll-bar-mode -1)
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

(when (boundp '+saved-load-path-during-dump)
  (global-font-lock-mode +1)
  (transient-mark-mode +1))

(when (display-graphic-p)
  (modify-all-frames-parameters
   '((right-divider-width . 0)
     (internal-border-width . 15)))

  (dolist (face '(window-divider
                  window-divider-first-pixel
                  window-divider-last-pixel))
    (face-spec-reset-face face)
    (set-face-foreground face (face-attribute 'default :background)))
  (set-face-background 'fringe (face-attribute 'default :background))
  (custom-set-faces
   '(mode-line ((t (:background "#c8c8c8" :foreground "#000000" :slant italic :box (:line-width 5 :color "#c8c8c8")))))
   '(mode-line-inactive ((t (:background "#e6e6e6" :foreground "#000000" :slant italic :box (:line-width 5 :color
                                                                                                         "#e6e6e6"))))))
  (ignore-errors (set-frame-font "Courier Prime 12"))
  )

;; mode-line settings
(use-package minions
  :config
  (minions-mode))

(global-display-line-numbers-mode)

(setq column-number-mode t) ; Show column number in the mode-line

(setq-default frame-title-format "%b [ %f ]")

(global-visual-line-mode t)

(provide 'chin-ui)
