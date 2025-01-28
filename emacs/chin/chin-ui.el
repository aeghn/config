;;; -*- lexical-binding: t; -*-

(load-theme 'modus-operandi)

;; Mode-line settings
;; (add-hook 'after-init-hook #'column-number-mode)
(setq mode-line-percent-position '(-3 "%p"))
(setq mode-line-position-column-line-format '(" %lL,%cC")) ; Emacs 28
(setq mode-line-defining-kbd-macro
      (propertize " Macro" 'face 'mode-line-emphasis))
(defun chin/set-mode-line ()
  (let ((focus-bg "#d5d5d5")
        (bg "#e0e0e0")
        (focus-fg "#202020")
        (fg "#404040"))
    (custom-set-faces
     `(mode-line ((t (:background ,focus-bg :foreground ,focus-fg :box (:line-width 4 :color ,focus-bg)))))
     `(mode-line-inactive ((t (:background ,bg :foreground ,fg :box (:line-width 4 :color ,bg))))))))

;; Seamlessly copied from: https://github.com/minad/org-modern
;; Add frame borders and window dividers
(modify-all-frames-parameters
 '((right-divider-width . 0)
   (internal-border-width . 20)))

(defun chin/set-fonts ()
  (set-face-attribute 'default nil
                      :family "Sarasa Mono SC" :height 108 :weight 'Regular))

(defun chin/set-divider ()
  (dolist (face '(window-divider
                  window-divider-first-pixel
                  window-divider-last-pixel))
    (face-spec-reset-face face)
    (set-face-foreground face (face-attribute 'default :background)))
  (set-face-background 'fringe (face-attribute 'default :background)))

(defun chin/tweak-gui ()
  (when (display-graphic-p)
    (chin/set-mode-line)
    (chin/set-fonts)
    (chin/set-divider)))

(add-hook 'after-make-frame-functions
          (lambda (frame)
            (select-frame frame)
            (chin/tweak-gui)))

(chin/tweak-gui)
;; Set tab width
(setq-default tab-width 4)
;; use space to indent by default
(setq-default indent-tabs-mode nil)


         ;;; Frame settings
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)
(menu-bar-mode -1)

(setq use-dialog-box nil
      use-short-answers t)

;; Scrolling Settings
(scroll-bar-mode -1)
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

;; Thanks to Daniel Mendler for this!  It removes the square brackets
;; that denote recursive edits in the modeline.  I do not need them
;; because I am using Daniel's `recursion-indicator':
;; <https://github.com/minad/recursion-indicator>.
(setq-default mode-line-modes
              (seq-filter (lambda (s)
                            (not (and (stringp s)
                                      (string-match-p
                                       "^\\(%\\[\\|%\\]\\)$" s))))
                          mode-line-modes))
;; Frame title settings
(setq frame-title-format "Emacs - %b  %f")
(setq backup-directory-alist `(("." . "~/.emacs-saves")))

;; Paren Settings
(show-paren-mode)
(setq show-paren-style 'mixed
      show-paren-content-when-offscreen 'overlay)


(setq word-wrap-by-category t)

(use-package git-gutter
  :config
  (global-git-gutter-mode +1))

(provide 'chin-ui)
