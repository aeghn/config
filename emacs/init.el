;;; -*- lexical-binding: t; -*-

;;; Custom file Settings
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)



;;; Package Settings
(require 'package)
(setq-default tsinghua-mirror
              '(("gnu"   . "https://mirrors.bfsu.edu.cn/elpa/gnu/")
                ("melpa" . "https://mirrors.bfsu.edu.cn/elpa/melpa/")))

(setq package-archives tsinghua-mirror)
(add-to-list 'package-archives
             '("nongnu" . "https://elpa.nongnu.org/nongnu/"))

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

;; Packages Initialization
(defun chin/ensure-all-packages ()
  (interactive)
  (let* ((packages '(dash magit-section markdown-mode with-editor modus-themes json-mode go-imenu go-mode vala-mode cargo cargo-mode caroline-theme org-download embark-consult embark magit expand-region restclient lua-mode ahk-mode corfu symbol-overlay consult all-the-icons xr ibuffer-project graphviz-dot-mode htmlize xcscope rust-mode ripgrep rainbow-mode meson-mode grey-paper-theme comment-dwim-2 vertico consult vundo org-superstar diff-hl ob-rust vertico  iscroll )))
    (package-refresh-contents)
    (dolist (p packages)
      (if (package-installed-p p)
          (package-upgrade p)
        (package-install p)))))

(chin/load-other-file "point-stack.el")
(chin/load-other-file "base-sidebar.el")
(chin/load-other-file "org-static-blog.el")
(chin/load-other-file "ibuffer-sidebar.el")
(chin/load-other-file "speed-sidebar.el")
(chin/load-other-file "envir.el")
(chin/load-other-file "mdired.el")
(chin/load-other-file "damer.el")
(chin/load-other-file "tool-bar.el")
(chin/load-other-file "shortcut.el")

;;; Platform Settings
(defconst chin/is-linux   (eq system-type 'gnu/linux))
(defconst chin/is-windows (memq system-type '(cygwin windows-nt ms-dos)))
(defconst chin/is-android (string-match-p "-linux-android$" system-configuration))

;; Windows-nt specific settings
;; windows-loader
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
    (setq-default package-gnupghome-dir (string-replace "c:/" "/c/" (expand-file-name "gnupg" package-user-dir)))
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
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)
(menu-bar-mode -1)

(setq use-dialog-box nil)
(setq use-short-answers t)

;; Avoid the ask, just visit the direct file.
(setq vc-follow-symlinks nil)

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

;; Theme Settings
(load-theme 'modus-operandi)
;; (load-theme 'modus-vivendi-tinted)


;; Font settings
(defun chin/set-fonts ()
  (when (display-graphic-p)
    (let ((prefered-mono-font-list '("Martian Mono" "IBM Plex Mono" "Jetbrains Mono"))
          (prefered-chinese-font-list '("Zhuque Fangsong (technical preview)"  "Noto Serif CJK SC"))
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
                          :family prefered-mono-font :height 108 :weight 'Regular)
      (set-face-attribute 'mode-line nil
                          :family prefered-serif-font :height 108 :weight 'Bold)
      (set-face-attribute 'mode-line-inactive nil
                          :family prefered-serif-font :height 108 :weight 'Regular)

      (setq ibuffer-sidebar-use-custom-font t)
      (setq ibuffer-sidebar-face `(:family "Archivo" :height 120))
      (setq speed-sidebar-face `(:family "Archivo" :height 120)))))

;; Toolbar Settings
(tool-bar-mode 1)
(setq tool-bar-button-margin 8
      tool-bar-images-pixel-height 100)

(defvar chin/show-tool-bar 1)
(defun chin/replace-tool-bar (&optional not-show)
  (let ((fw (frame-native-width))
        (fh (frame-native-height))
        (tp (frame-parameter (selected-frame) 'tool-bar-position))
        (pos))
    (when (or not-show (not chin/show-tool-bar))
      (setq chin/show-tool-bar nil)
      )
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
(setq frame-title-format "Emacs - %b  %f")
(setq backup-directory-alist `(("." . "~/.emacs-saves")))
(setq speedbar-show-unknown-files t)

;;; Sidebar Settings
;; Ibuffer Sidebar Settings
(global-set-key (kbd "C-x C-b") 'ibuffer-sidebar-focus-or-toggle)
(global-set-key (kbd "C-x <left>") 'ibuffer-sidebar-previous-buffer)
(global-set-key (kbd "C-x <right>") 'ibuffer-sidebar-next-buffer)
(global-set-key (kbd "C-x C-<left>") 'ibuffer-sidebar-previous-buffer)
(global-set-key (kbd "C-x C-<right>") 'ibuffer-sidebar-next-buffer)
;; (global-set-key (kbd "M-p") 'ibuffer-sidebar-previous-buffer)
;; (global-set-key (kbd "M-n") 'ibuffer-sidebar-next-buffer)
(global-set-key (kbd "M-j") 'base-sidebar-select-window)

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
(require 'org-tempo)
(setq org-special-ctrl-a/e t
      ;; Edit settings
      ;; org-auto-align-tags nil
      org-tags-column 0
      org-catch-invisible-edits 'show-and-error

      ;; org-insert-heading-respect-content t

      org-src-tab-acts-natively nil

      org-adapt-indentation t
      org-hide-leading-stars t
      ;; org-odd-levels-only t

      ;; Org styling, hide markup etc.
      org-hide-emphasis-markers nil
      org-pretty-entities t
      org-ellipsis " ⋯ "

      org-export-preserve-breaks t

      org-confirm-babel-evaluate nil)
(setq org-latex-listings t)

;; ;; Make deletion(obsolote) text foreground with dark gray.
;; (add-to-list 'org-emphasis-alist
;;              '("+" (:foreground "dark gray"
;;                                 :strike-through t)))
;; ;; Make code style around with box.
;; (add-to-list 'org-emphasis-alist
;;              '("~" (:box (:line-width 1
;;                                       :color "grey75"
;;                                       :style released-button))))

(require 'org-superstar)
(setq org-superstar-leading-bullet ?\s)
(setq org-superstar-headline-bullets-list '(?⁂ ))
(setq org-superstar-item-bullet-alist '((?* . ?⁑) (?+ . ?〄) (?- . ?☁)))
;; 🅾🄮 ⃝⃞ℓℵ⇒∀∂∃∅∆∇∈∉∊∋∽≌≒⊠⊿⏃⏄⏅⏛▷◯◉●◎◇◈◯♦♥♭♮♯⚠⚽⚾❀✿✽❖⦿〇〠 ?☁ ?∭ ?∬ ?∫ ?∮

(custom-set-faces
 '(speedbar-directory-face ((t (:foreground "#aa0000" :weight normal))))
 '(org-level-1 ((t (:weight normal :height 1.3 ))))
 '(org-level-2 ((t (:weight normal :height 1.2 ))))
 '(org-level-3 ((t (:weight normal :height 1.1 ))))
 '(org-level-4 ((t (:weight normal :height 1.0 ))))
 '(org-level-5 ((t (:weight normal :height 1.0 ))))
 '(org-level-6 ((t (:weight normal :height 1.0 ))))
 '(org-level-8 ((t (:weight normal))))
 '(org-block-begin-line ((t (:height 0.8 :slant italic))))
 '(org-block-end-line ((t (:inherit 'org-block-begin-line))))
 '(org-superstar-header-bullet ((t (:weight normal :foreground "#aa0000" :height 1.0))))
 '(org-superstar-item ((t (:weight normal :foreground "#4466ee" :height 1.0)))))
;; ㊟

;; Agenda styling
(setq org-log-done 'time)
(setq org-todo-keywords
      '((sequence "TODO(t)" "STARTED(s)" "WAIT(w)" "|" "DONE(d)" "CANCELED(c)")))

(defun chin/org-hook-function ()
  (interactive)
  (let ((variable-font "Zhuque Fangsong (technical preview)")
        (mono-font "IBM Plex Mono"))
    ;; (setq-local face-remapping-alist `((default (:family ,variable-font) variable-pitch)
    ;;                                    (org-code (:family ,mono-font) org-code)
    ;;                                    (org-verbatim (:family ,mono-font) org-verbatim)
    ;;                                    (org-block (:family ,mono-font) org-block)
    ;;                                    (org-block-begin-line (:family ,mono-font) org-block)))
    (setq line-spacing 0.1))
  (org-superstar-mode 1)
  (olivetti-mode)
  (define-key org-mode-map (kbd "M-h") 'chin/delete-blanks))

;; Copied from https://github.com/lijigang/100-questions-about-orgmode

(setq org-plantuml-exec-mode 'plantuml)
;; https://orgmode.org/worg/org-contrib/babel/languages/ob-doc-plantuml.html
(org-babel-do-load-languages
 'org-babel-load-languages
 '((plantuml . t)
   (python . t)
   (rust . t)
   (dot . t)))

(add-to-list
 'org-src-lang-modes '("plantuml" . plantuml))

(defvar chin/org-managed-files
  (if chin/is-windows
      "E:\\files\\docs\\org"
      "~/files/docs/org"))

(defun chin/org-files ()
  (interactive)
  (find-file
   (completing-read "Org files: " (directory-files chin/org-managed-files nil ".*org$"))))

(defun chin/org-file-open ()
  (interactive)
  (let* ((date (format-time-string "%y-%m-%d"))
         (result (completing-read "Org files: " (directory-files chin/org-managed-files nil ".*\\.org")))
         (full-result (expand-file-name result chin/org-managed-files)))
    (if (file-exists-p full-result)
        (find-file full-result)
      (find-file (expand-file-name (concat date "-" (replace-regexp-in-string ".org$" "" result) ".org") chin/org-managed-files)))))

(when (boundp 'diff-hl-mode)
  (add-hook 'prog-mode-hook 'diff-hl-mode)
  (add-hook 'org-mode-hook 'diff-hl-mode)
  )
(require 'iscroll)
(add-hook 'org-mode-hook 'iscroll-mode)
(add-hook 'org-mode-hook 'chin/org-hook-function)

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
(global-set-key (kbd "M-s l") 'consult-line)

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

(defun chin/delete-blanks (&optional insert-blank-p)
  (interactive)
  (let* ((end-pos (progn (back-to-indentation)
                         (point)))
         (start-pos (1+ (search-backward-regexp "[^\n[:space:]]"))))
    (delete-region start-pos end-pos)
    (forward-char)
    (unless insert-blank-p
      (insert " "))))

(global-set-key (kbd "M-h") 'chin/delete-blanks)
(global-set-key (kbd "C-M-h") (lambda () (interactive) (chin/delete-blanks t)))

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

(defvar chin/cangjie-fancha-file "/home/chin/files/docs/others/cangjie.fancha")
(defvar chin/cangjie-fancha-file-hist "/home/chin/files/docs/others/cangjie.fancha.history")
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
    (let ((option (completing-read "Cangjie: " list)))
      (write-region (concat (format-time-string "%Y-%m-%d %H:%M:%S | ") option "\n") nil chin/cangjie-fancha-file-hist t))))

(global-set-key (kbd "C-;") 'chin/cangjie-fancha)

;; Eglot
(require 'eglot)
(define-key eglot-mode-map (kbd "M-RET") 'eglot-code-actions)
(add-hook 'rust-mode-hook 'eglot-ensure)
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

;; Ansi Shell
(defun chin/toggle-ansi-term ()
  (interactive)
  (if-let* ((name "bottom-ansi-shell")
            (buffer (get-buffer (concat "*" name "*")))
            (window (get-buffer-window buffer)))
      (delete-window window)
    (setq window (split-window-below (- (frame-height) 12) (frame-root-window)))
    (select-window window)
    (if buffer
        (switch-to-buffer buffer)
      (ansi-term "/bin/zsh" name))))

(global-set-key (kbd "M-=") 'chin/toggle-ansi-term)
(require 'term)
(define-key term-raw-map (kbd "M-=") 'chin/toggle-ansi-term)


(with-eval-after-load 'project
  (add-to-list 'project-vc-ignores ".ccls-cache/")
  (add-to-list 'project-vc-ignores "node_modules")
  (defvar project-language-aware-root-files
    '("tsconfig.json"
      "package.json"
      "Cargo.toml"
      "compile_commands.json"
      "project.clj"
      "compile_flags.txt"))
  (defun project-try-language-aware (dir)
    "Find a super-directory of DIR containing a root file."
    (let ((dir (cl-loop for pattern in project-language-aware-root-files
                        for result = (locate-dominating-file dir pattern)
                        if result return result)))
      (and dir (cons 'language-aware dir))))
  (cl-defmethod project-root ((project (head language-aware)))
    (cdr project))
  (add-hook 'project-find-functions
            #'project-try-language-aware))

(require 'tramp)

(require 'savehist)
(savehist-mode)


(with-eval-after-load 'ox-latex
  ;; http://orgmode.org/worg/org-faq.html#using-xelatex-for-pdf-export
  ;; latexmk runs pdflatex/xelatex (whatever is specified) multiple times
  ;; automatically to resolve the cross-references.
  (setenv "TEXMFHOME" "/home/chin/Repos/tex_config/texmfhome")
  (setq org-latex-pdf-process '("xelatex -interaction=batchmode -shell-escape -f %f")
        org-latex-remove-logfiles t
        org-latex-logfiles-extensions '("aux" "bcf" "blg" "fdb_latexmk" "fls"
                                        "figlist" "idx" "log" "nav" "out" "ptc"
                                        "run.xml" "snm" "toc" "vrb" "xdv"))


  ;; update the list of LaTeX classes and associated header (encoding, etc.)
  ;; and structure
  (add-to-list 'org-latex-classes
               `("beamer"
                 ,(concat "\\documentclass[presentation]{beamer}\n"
                          "[DEFAULT-PACKAGES]"
                          "[PACKAGES]"
                          "[EXTRA]\n")
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))

  (add-to-list 'org-latex-classes
               '("elegantpaper"
                 "\\documentclass[lang=cn]{elegantpaper}
                  \\bigskip
                 [NO-DEFAULT-PACKAGES]
                 [PACKAGES]
                 [EXTRA]"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
  (setq org-latex-listings 'minted)
  (add-to-list 'org-latex-packages-alist '("" "minted")))

(require 'ox-latex)
;; (fido-vertical-mode)

(global-visual-line-mode)
(setq word-wrap-by-category t)

(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
(setq kill-whole-line t)


(when (file-exists-p "~/Projects/blog/tools/org-static-blog.el")
  (load-file "~/Projects/blog/tools/org-static-blog.el")
  (load-file "~/Projects/blog/tools/envir.el"))


(defun chin/insert-half-width-space-between-chinese-and-english ()
  "在中英文之间增加半角空格"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "\\([a-zA-Z0-9]\\)\\([^\x00-\xff]\\)" nil t)
      (replace-match "\\1 \\2"))
    (goto-char (point-min))
    (while (re-search-forward "\\([^\x00-\xff]\\)\\([a-zA-Z0-9]\\)" nil t)
      (replace-match "\\1 \\2"))))
