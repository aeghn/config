 ;;; -*- lexical-binding: t; -*-

;;; Custom file Settings
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;;; Package Settings
(require 'package)
(setq-default tsinghua-mirror
              '(("gnu"   . "https://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                ("melpa" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
                ("org"   . "https://mirrors.tuna.tsinghua.edu.cn/elpa/org/")))

(setq package-archives tsinghua-mirror)

(package-initialize)


;; Seamlessly stolen from https://github.com/rejeep/f.el/blob/master/f.el
(defun chin/this-true-file ()
  "Return path to this file."
  (let ((true-file))
    (setq true-file
          (cond (load-in-progress load-file-name)
                ((and (boundp 'byte-compile-current-file) byte-compile-current-file)
                 byte-compile-current-file)
                (:else (buffer-file-name))))
    (file-truename true-file)))

(defun chin/load-other-file (filename)
  (let ((f (if (file-exists-p filename)
               filename
             (expand-file-name
              filename (file-name-directory (chin/this-true-file))))))
    (message "Try to load file: %s" f)
    (when (file-exists-p f)
      (load (file-truename f)))))

(add-to-list
 'image-load-path
 (expand-file-name "lib/images" (file-name-directory (chin/this-true-file))))

(chin/load-other-file "point-stack.el")
(chin/load-other-file "chin-bw-utils.el")
(chin/load-other-file "org-static-blog.el")
(chin/load-other-file "ibuffer-sidebar.el")
(chin/load-other-file "speed-sidebar.el")
(chin/load-other-file "envir.el")
(chin/load-other-file "mdired.el")
(chin/load-other-file "damer.el")
(chin/load-other-file "tool-bar.el")

;;; Platform Settings
(defconst chin/is-linux   (eq system-type 'gnu/linux))
(defconst chin/is-windows (memq system-type '(cygwin windows-nt ms-dos)))
(defconst chin/is-android (string-match-p "-linux-android$" system-configuration))

;; Windows-nt specific settings
(when chin/is-windows
  (let ((msys2root "C:\\msys64\\"))
    (setenv "WENV" "D:\\wenv")
    (setenv "PATH" (concat
                    ;; Remember to install `mingw-w64-x86_64-gnupg'
                    "d:\\wenv\\bin;"
                    msys2root "mingw64\\bin" ";"
                    msys2root "mingw64\\x86_64-w64-mingw32\\bin" ";"
                    msys2root "usr\\bin" ";"
                    (getenv "PATH")))
    ;; Without this the new added $PATH value won't be inherite by exec-path
    (setq exec-path (split-string (getenv "PATH") path-separator))
    (setq default-directory "e:/")))

;; Locale Settings
(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))
(prefer-coding-system 'utf-8)
(setq system-time-locale "C")
(unless (eq system-type 'windows-nt)
  (set-selection-coding-system 'utf-8))

;;; Frame settings
(setq-default truncate-lines nil)
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)
(menu-bar-mode -1)

(setq truncate-partial-width-windows nil)
(setq use-dialog-box nil)
(setq use-short-answers t)

;; Scrolling Settings
(scroll-bar-mode -1)
(setq scroll-step           1
      scroll-conservatively 10000)

;; Disable the annoying bell.
(setq ring-bell-function 'ignore)
(pixel-scroll-precision-mode t)

;; Theme Settings
(load-theme 'modus-operandi)
;; (load-theme 'spacemacs-dark)
;; (load-theme 'modus-vivendi)

;; Font settings
(defun chin/set-fonts ()
  (when (display-graphic-p)
    (let ((prefered-mono-font-list '("Jetbrains Mono"))
          (prefered-chinese-font-list '("Noto Serif CJK CN"))
          (prefered-serif-font-list (list "Literata 7pt" "Charter" "Roboto"))
          (first-font-fun (make-symbol "chin/get-first-available-font"))
          prefered-mono-font prefered-chinese-font prefered-serif-font )
      (fset first-font-fun
            (lambda (font-list selected-font)
              (if selected-font
                  selected-font
                (let* ((length (length font-list))
                       (font-name)
                       (i 0))
                  (while (and (< i length) (not font-name))
                    (setq font-name (nth i font-list))
                    (unless (find-font (font-spec :name font-name))
                      (setq font-name nil))
                    (setq i (+ i 1)))
                  font-name))))
      (setq prefered-mono-font
            (funcall first-font-fun prefered-mono-font-list prefered-mono-font))
      (setq prefered-chinese-font
            (funcall first-font-fun prefered-chinese-font-list prefered-chinese-font))
      (setq prefered-serif-font
            (funcall first-font-fun prefered-serif-font-list prefered-serif-font))
      (set-fontset-font "fontset-default" '(#xe000 . #xf8ff) "nrss")
      (set-face-attribute 'default nil
                          :family prefered-mono-font :height 120 :weight 'Regular)
      (set-face-attribute 'mode-line nil
                          :family prefered-serif-font :height 120 :weight 'Bold)
      (set-face-attribute 'mode-line-inactive nil
                          :family prefered-serif-font :height 120 :weight 'Regular)
      (setq ibuffer-sidebar-use-custom-font t)
      (setq ibuffer-sidebar-face `(:family "Public Sans" :height 108))
      (setq speed-sidebar-face `(:family "Public Sans" :height 108))
      )))

;; Toolbar Settings
(tool-bar-mode 1)
(setq tool-bar-button-margin 8
      tool-bar-images-pixel-height 100)

(defvar chin/show-tool-bar t)
(defun chin/replace-tool-bar (&optional not-show)
  (let ((fw (frame-native-width))
        (fh (frame-native-height))
        (tp (frame-parameter (selected-frame) 'tool-bar-position))
        (pos))
    (when (or not-show)
      (setq chin/show-tool-bar nil))
    (cond ((not chin/show-tool-bar) (when tool-bar-mode (tool-bar-mode -1)))
          ((< fh 500) (when tool-bar-mode (tool-bar-mode -1)))
          ((< (* fh 5) (* fw 3))
           (unless (string= tp 'left)
             (set-frame-parameter nil 'tool-bar-position 'left))
           (unless tool-bar-mode (tool-bar-mode 1)))
          ((> fh fw)
           (unless (string= tp 'top)
             (set-frame-parameter nil 'tool-bar-position 'top))
           (unless tool-bar-mode (tool-bar-mode 1))))))

(add-hook 'window-state-change-hook #'chin/replace-tool-bar)

;; Mode-line settings
(add-hook 'after-init-hook #'column-number-mode)
(setq mode-line-percent-position '(-3 "%p"))
(setq mode-line-position-column-line-format '(" %lL,%cC")) ; Emacs 28
(setq mode-line-defining-kbd-macro
      (propertize " Macro" 'face 'mode-line-emphasis))

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

;; Seamlessly copied from: https://github.com/minad/org-modern
;; Add frame borders and window dividers
(modify-all-frames-parameters
 '((right-divider-width . 0)
   (internal-border-width . 20)))

(defun chin/set-divider ()
  (dolist (face '(window-divider
                  window-divider-first-pixel
                  window-divider-last-pixel))
    (face-spec-reset-face face)
    (set-face-foreground face (face-attribute 'default :background)))
  (set-face-background 'fringe (face-attribute 'default :background)))

(defun chin/set-gui-emacs ()
  (when (display-graphic-p)
    (progn
      (chin/set-fonts)
      (chin/set-divider))))

(add-hook 'after-make-frame-functions
          (lambda (frame)
            (select-frame frame)
            (chin/set-gui-emacs)))

(chin/set-gui-emacs)

;; Frame title settings
(setq frame-title-format "%b [%f] -- GNU/Emacs")
(setq backup-directory-alist `(("." . "~/.emacs-saves")))
(setq speedbar-show-unknown-files t)

;;; Sidebar Settings
;; Ibuffer Sidebar Settings
(global-set-key (kbd "C-x C-b") 'ibuffer-sidebar-focus-or-toggle)
(global-set-key (kbd "C-x <left>") 'ibuffer-sidebar-previous-buffer)
(global-set-key (kbd "C-x <right>") 'ibuffer-sidebar-next-buffer)
(global-set-key (kbd "C-x C-<left>") 'ibuffer-sidebar-previous-buffer)
(global-set-key (kbd "C-x C-<right>") 'ibuffer-sidebar-next-buffer)
(global-set-key (kbd "M-p") 'ibuffer-sidebar-previous-buffer)
(global-set-key (kbd "M-n") 'ibuffer-sidebar-next-buffer)
(global-set-key (kbd "M-j") 'ibuffer-sidebar-select-window)

;; Speed Sidebar Settings
(global-set-key (kbd "M-l") 'speed-sidebar-focus-or-toggle)

;; Paren Settings
(show-paren-mode)
(setq show-paren-style 'mixed
      show-paren-content-when-offscreen 'overlay)

(defun chin/match-paren (arg)
  "Go to the matching paren if on a paren; otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))

(global-set-key (kbd "C-c '") 'chin/match-paren)

;; CC mode
(add-hook 'c-mode-hook
          (lambda ()
            (require 'xcscope)
            (local-set-key (kbd "C-c ,") 'cscope-find-functions-calling-this-function)
            (local-set-key (kbd "C-c .") 'cscope-find-global-definition-no-prompting)))

;; Org-mode settings
(require 'org)
(setq
 ;; Edit settings
 ;; org-auto-align-tags nil
 org-tags-column 0
 org-catch-invisible-edits 'show-and-error
 org-special-ctrl-a/e t
 org-insert-heading-respect-content t

 org-adapt-indentation t
 org-hide-leading-stars t
 ;; org-odd-levels-only t

 ;; Org styling, hide markup etc.
 org-hide-emphasis-markers nil
 org-pretty-entities t
 org-ellipsis "...")

;; Agenda styling
(setq org-log-done 'time)
(setq org-todo-keywords
      '((sequence "TODO(t)" "STARTED(s)" "WAIT(w)" "|" "DONE(d)" "CANCELED(c)")))

(defun chin/insert-image-from-clipboard ()
  (interactive)
  (let* ((pure-filename (file-name-sans-extension
                         (file-name-nondirectory
                          (buffer-file-name))))
         (time (format-time-string "%y%m%d-%H%M%S"))
         (image-dir-name "images")
         (image-dir (expand-file-name image-dir-name))
         (filename (concat pure-filename "-" time ".png")))
    (unless (file-exists-p image-dir)
      (make-directory image-dir))
    (if (process-file "convert" nil nil nil "clipboard:myimage"
                      (expand-file-name filename image-dir))
        (insert  (concat "[[file:./" image-dir-name "/" filename "]]"))
      (message "Unable to create image"))))
(define-key org-mode-map (kbd "C-c p") 'chin/insert-image-from-clipboard)

(global-set-key
 (kbd "M-5")
 (lambda ()
   (interactive)
   (find-file "/home/chin/files/docs/todo.org")))

;; Set tab width
(setq-default tab-width 4)
;; use space to indent by default
(setq-default indent-tabs-mode nil)

;; Misc settings
;; Auto save files after losing focus.
;; https://emacs.stackexchange.com/questions/60970/how-to-replace-focus-out-hook-with-after-focus-change-function-in-emacs-27
(add-function :after after-focus-change-function
              (lambda () (save-some-buffers t)))

;; Completion Settings
(require 'vertico)
(require 'consult)
(vertico-mode)
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

;; History files
;; Recentf Settings
(require 'recentf)
(recentf-mode 1)

(defconst chin/hist-files-file "~/.hist-files")

(defun chin/add-visited-file ()
  (write-region (concat (buffer-file-name) "\n") nil chin/hist-files-file t))

(defun chin/read-visited-files ()
  (dolist (f (split-string
              (with-temp-buffer
                (insert-file-contents chin/hist-files-file)
                (buffer-substring-no-properties (point-min) (point-max)))
              "\r?\n"
              t))
    (recentf-add-file f)))

(ignore-errors
  (chin/read-visited-files))
(add-hook 'find-file-hook 'chin/add-visited-file)

;; Comment Settings
(global-set-key (kbd "M-;") 'comment-dwim-2)

;; Dired Settings
(defadvice dired-find-file (around dired-find-file-single-buffer activate)
  "Replace current buffer if file is a directory."
  (interactive)
  (let ((orig (current-buffer)) (filename (dired-get-file-for-visit)))
    ad-do-it (when (and (file-directory-p filename) (not (eq (current-buffer) orig)))
               (kill-buffer orig))))

;; Power Settings
(defun chin/server-shutdown ()
  "Save buffers, quit, and shutdown (kill) server."
  (interactive)
  (save-some-buffers)
  (kill-emacs))

;; Buffer Shortcut Functions
(defun chin/indent-current-buffer ()
  "indent current buffer"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))

(global-set-key (kbd "C-c i") 'chin/indent-current-buffer)

(defun chin/delete-blanks ()
  (interactive)
  (let* ((end-pos (progn (back-to-indentation)
                         (point)))
         (start-pos (1+ (search-backward-regexp "[^\n[:space:]]"))))
    (delete-region start-pos end-pos)
    (forward-char)
    (insert " ")))

(global-set-key (kbd "M-h") 'chin/delete-blanks)

(define-advice set-mark-command (:before-while (arg))
  "Repeat C-SPC to expand region."
  (interactive "P")
  (if (eq last-command 'set-mark-command)
      (progn
        (er/expand-region 1)
        nil)
    t))

(defun chin/revert-buffer ()
  "Revert buffer without confirming."
  (interactive)
  (revert-buffer t t t)
  (message "buffer is reverted"))

(global-set-key (kbd "M-r") 'chin/revert-buffer)

(defun chin/insert-tab-char ()
  "Insert a tab char. (ASCII 9, \t)"
  (interactive)
  (insert "\t"))

(global-set-key (kbd "C-t") 'chin/insert-tab-char)

(defun chin/move-beginning-of-line ()
  "Move point back to indentation of beginning of line or beginning of line."
  (interactive)
  (let ((orig-begin (point)))
    (back-to-indentation)
    (if (= orig-begin (point))
        (beginning-of-line))))

(global-set-key (kbd "C-a") 'chin/move-beginning-of-line)

;; Message Helper
(defun chin/message-toggle ()
  "Toggle a persistent message popup window.
If popup is visible but unselected, select it.
If popup is focused, kill it."
  (interactive)
  (if-let ((win (get-buffer-window "*Messages*")))
      (if (eq (selected-window) win)
          ;; If users attempt to delete the sole ordinary window, silence it.
          (ignore-errors (delete-window win))
        (select-window win))
    (switch-to-buffer (get-buffer "*Messages*"))
    (goto-char (point-max))))

(global-set-key (kbd "M-`") 'chin/message-toggle)

(defun chin/raise-or-suspend-frame ()
  (if (frame-focus-state)
      (suspend-frame)
    (raise-frame)))

;; Back Or Forward Settings
(require 'point-stack)
(point-stack-setup-advices)
(global-set-key (kbd "M-1") 'point-stack-pop)
(global-set-key (kbd "M-2") 'point-stack-forward-stack-pop)

;; ;; Chinese
(chin/load-other-file "cangjie.el")
(set-input-method 'chinese-cns-cangjie)
(deactivate-input-method)
(put 'erase-buffer 'disabled nil)

(defvar chin/cangjie-fancha-file "/home/chin/files/docs/cangjie.fancha")
(when chin/is-windows
  (setq chin/cangjie-fancha-file "e:/files/docs/cangjie.fancha"))
(defun chin/cangjie-fancha ()
  (interactive)
  (let ((list (split-string
               (with-temp-buffer
                 (insert-file-contents chin/cangjie-fancha-file)
                 (buffer-substring-no-properties (point-min) (point-max)))
               "\r?\n"
               t)))
    (completing-read "Cangjie: " list)))

;; Eglot
(require 'eglot)
(define-key eglot-mode-map (kbd "M-RET") 'eglot-code-actions)
(add-hook 'rust-mode-hook 'eglot-ensure)

(require 'corfu)
(setq corfu-auto t)
(setq corfu-quit-at-boundary t)
(global-corfu-mode)
(put 'downcase-region 'disabled nil)

;; Message Functions
(defun chin/clear-buffer-forcily ()
  (interactive)
  (let ((buffer-read-only nil))
    (erase-buffer)))

;; Damer
(defun damer-default ()
  (interactive)
  (damer "/home/chin/files/datamanager.db"))


;; Ansi Shell
(defun chin/toggle-ansi-term ()
  (interactive)
  (if-let* ((name "bottom-ansi-shell")
            (buffer (get-buffer (concat "*" name "*")))
            (window (get-buffer-window buffer)))
      (delete-window window)
    (setq window (split-window-below (- (frame-height) 12) (frame-root-window)))
    (select-window window)
    (ansi-term "/bin/zsh" name)))

(global-set-key (kbd "M-=") 'chin/toggle-ansi-term)
