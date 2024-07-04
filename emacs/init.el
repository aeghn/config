;;; -*- lexical-binding: t; -*-

;;; Custom file Settings
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;;; Loading Tools
;;; Platform Settings
(defconst chin/is-linux   (eq system-type 'gnu/linux))
(defconst chin/is-windows (memq system-type '(cygwin windows-nt ms-dos)))
(defconst chin/is-android (string-match-p "-linux-android$" system-configuration))

(defmacro chin/when-nt! (&rest body)
  "Execute BODY if running on a Windows system."
  `(when chin/is-windows
     ,@body))

;; Seamlessly stolen from https://github.com/rejeep/f.el/blob/master/f.el
(defun chin/true-file ()
  "Return path to this file."
  (let ((true-file))
    (setq true-file
          (cond (load-in-progress load-file-name)
                ((and (boundp 'byte-compile-current-file) byte-compile-current-file)
                 byte-compile-current-file)
                (:else (buffer-file-name))))
    (file-truename true-file)))

(defun chin/load-file (filename)
  (if (file-exists-p filename)
      (load (file-truename filename))
    (let ((p (expand-file-name filename (file-name-directory (chin/true-file)))))
      (if (file-exists-p p)
          (load p)
        nil))))

(require 'cl-lib)

(cl-defmacro block! (title &rest body &key packages &allow-other-keys)
  "Ensure PACKAGES are installed and execute BODY with TITLE.
   TITLE is a string for logging.
   PACKAGES is a list of packages to be ensured installed.
   BODY is the code to execute after ensuring packages are installed."
  (while (keywordp (car body))
    (setq body (cdr (cdr body))))
  (let ((ensure-packages
         (mapcar (lambda (pn)
                   `(let ((pkg (intern ,pn)))
                      (unless (package-installed-p pkg)
                        (unless (chin/load-file ,pn)
                          (package-refresh-contents)
                          (package-install pkg)))))
                 packages)))
    `(condition-case err
         (progn
           ,@ensure-packages
           ,@body)
       (error (message "An error occurred while executing body in %s, %s"
                       ,title err)))))

;;; Settings
(block! "Packages Settings"
        (require 'package)
        (setq-default tsinghua-mirror
                      '(("gnu"   . "https://mirrors.bfsu.edu.cn/elpa/gnu/")
                        ("melpa" . "https://mirrors.bfsu.edu.cn/elpa/melpa/")))

        (setq package-archives tsinghua-mirror)
        (package-initialize)

        (chin/when-nt!
         ;; TODO remove this
         (setq package-check-signature nil)))

(block! "Path, Dir and Locale"
        ;; Define some basic variables here.
        ;; We should not specify any full path below, they should be the sub directory of
        ;; those basic dirs.
        (setq-default chin/playground-data-dir "~/playground/playground-data"
                      chin/docs-dir "~/files/docs")

        (chin/when-nt!
         ;; Windows-nt specific settings
         (setq chin/playground-data-dir "F:/playground-data"
               chin/docs-dir "D:/files/docs")
         (let ((msys2root "C:/msys64/"))
           (setenv "PATH" (concat
                           ;; Remember to install `mingw-w64-x86_64-gnupg'
                           "D:/tools/cmd;"
                           msys2root "mingw64/bin" ";"
                           msys2root "mingw64/x86_64-w64-mingw32/bin" ";"
                           msys2root "usr/bin" ";"
                           (getenv "PATH")))
           (setq package-gnupghome-dir (string-replace "c:/" "/c/" (expand-file-name "gnupg" package-user-dir)))
           ;; Without this the new added $PATH value won't be inherite by exec-path
           (setq exec-path (split-string (getenv "PATH") path-separator))))

        ;; Locale Settings
        (when (fboundp 'set-charset-priority)
          (set-charset-priority 'unicode))
        (prefer-coding-system 'utf-8)
        (setq system-time-locale "C"))

(block! "Gui Settings"
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
          (set-fontset-font "fontset-default" '(#xe000 . #xf8ff) "nrss")
          (set-face-attribute 'default nil
                              :family "Martian Mono" :height 108 :weight 'Regular))

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

        (chin/tweak-gui))

(block! "Org-mode Related Settings"
        ;; Org-mode settings
        (require 'org)
        (require 'org-tempo)
        (require 'org-tidy)
        ;; (require 'org-visual-indent)
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
              org-ellipsis " ... "

              org-export-preserve-breaks t

              org-confirm-babel-evaluate nil)
        (setq org-latex-listings t)

        ;; 🅾🄮 ⃝⃞ℓℵ⇒∀∂∃∅∆∇∈∉∊∋∽≌≒⊠⊿⏃⏄⏅⏛▷◯◉●◎◇◈◯♦♥♭♮♯⚠⚽⚾❀✿✽❖⦿〇〠 ?☁ ?∭ ?∬ ?∫ ?∮

        ;; Agenda styling
        (setq org-log-done 'time)
        (setq org-todo-keywords
              '((sequence "TODO(t)" "CURR(c)" "WAIT(w)" "|" "DONE(d)" "STOP(s)")))

        ;; Org Faces and Symbols
        (custom-set-faces
         '(speedbar-directory-face ((t (:foreground "#aa0000" :weight normal))))
         '(org-document-title ((t (:weight normal :height 2.0))))
         '(org-level-1 ((t (:weight normal :height 1.1 ))))
         '(org-level-2 ((t (:weight normal :height 1.05 ))))
         '(org-level-3 ((t (:weight normal :height 1.05 ))))
         '(org-level-4 ((t (:weight normal :height 1.0 ))))
         '(org-level-5 ((t (:weight normal :height 1.0 ))))
         '(org-level-6 ((t (:weight normal :height 1.0 ))))
         '(org-level-8 ((t (:weight normal)))))

        (defun chin/org-face-hook ()
          (let ((variable-font "Sarasa Mono SC"))
            (setq-local face-remapping-alist
                        `((default (:family ,variable-font :height 120) variable-pitch)
                          (org-block (:family "Martian Mono Nr Rg" :height 108) org-block))
                        line-spacing 0.2)
            (org-tidy-mode)))
        (defun chin/org-agenda-current-file ()
          (interactive)
          (unless (eq major-mode 'org-mode)
            (throw "Use Org-agenda-current-file in org-mode only!"))
          (let ((org-agenda-files (list (buffer-file-name))))
            (org-agenda)))

        (defun chin/insert-date ()
          (interactive)
          (insert (format-time-string "%y%m-%d ")))

        (defun chin/org-hook-function ()
          (chin/org-face-hook)
          (define-key org-mode-map (kbd "M-.") 'chin/insert-date)
          (define-key org-mode-map (kbd "M-h") 'chin/delete-blanks))


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

        (defvar chin/org-dir (expand-file-name "org" chin/docs-dir))
        (defun chin/org-file-open ()
          (interactive)
          (let* ((date (format-time-string "%y-%m-%d"))
                 (result (completing-read "Org files: " (directory-files chin/org-dir nil ".*\\.org")))
                 (full-result (expand-file-name result chin/org-dir)))
            (if (file-exists-p full-result)
                (find-file full-result)
              (find-file (expand-file-name (concat date "-"
                                                   (replace-regexp-in-string ".org$" "" result)
                                                   ".org")
                                           chin/org-dir)))))

        (defconst chin/todo-prefix (rx (seq
                                        bol
                                        (one-or-more "*")
                                        (opt " " (= 4 (any "A-Z")))
                                        (opt " " (seq (= 4 (any "0-9")) "-" (= 2 (any "0-9)"))))
                                        (opt " .")
                                        (opt " ")
                                        eol)))

        (defun chin/todo (dirpath filename)
          (interactive)
          (when-let ((buf (find-file (expand-file-name filename dirpath))))
            (with-current-buffer buf
              (let* ((today (format-time-string "%y%m-%d" (current-time))))
                (goto-char (point-min))
                (replace-regexp chin/todo-prefix "")
                (goto-char (point-max))
                (newline)
                (delete-blank-lines)
                (insert "* TODO " today " . ")))))

        (global-set-key
         (kbd "<f6>")
         (lambda ()
           (interactive)
           (chin/todo chin/org-dir "todo.org")))
        (global-set-key
         (kbd "<f5>")
         (lambda ()
           (interactive)
           (chin/todo chin/org-dir "todo-work.org")))

        (require 'iscroll)
        (add-hook 'org-mode-hook 'iscroll-mode)
        (add-hook 'org-mode-hook 'chin/org-hook-function)

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

        (block!
         "org-roam"
         :packages ("org-roam-helper.el" "org-roam")

         (require 'org-roam)
         (setq org-roam-directory chin/org-dir)
         (setq org-roam-extract-new-file-path "%<%y-%m-%d>-${slug}.org")
         (setq org-roam-capture-templates
               '(("d" "default" plain "%?"
                  :target (file+head "%<%y-%m-%d>-${slug}.org"
                                     "#+title: ${title}\n")
                  :unnarrowed t)))
         (add-hook 'after-init-hook 'org-roam-db-autosync-mode))

        (defun chin/org-insert-image-from-clipboard ()
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
        (define-key org-mode-map (kbd "C-c p") 'chin/insert-image-from-clipboard))

(block! "Completions"
        '("vertico" "consult" "corfu")

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

        (require 'corfu)
        (setq corfu-auto t)
        (setq corfu-quit-at-boundary t)
        (global-corfu-mode))

(block! "Comment"
        :packages ("comment-dwim-2")
        (global-set-key (kbd "M-;") 'comment-dwim-2))

(block! "Ui Settins"
        ;; Set tab width
        (setq-default tab-width 4)
        ;; use space to indent by default
        (setq-default indent-tabs-mode nil)


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
        (defun chin/revert-buffer ()
          "Revert buffer without confirming."
          (interactive)
          (revert-buffer t t t)
          (message "buffer is reverted"))

        (global-set-key (kbd "M-r") 'chin/revert-buffer)

        (defun chin/clear-buffer-forcily ()
          (interactive)
          (let ((buffer-read-only nil))
            (erase-buffer)))


        ;; Message Helper
        (defun chin/message-toggle ()
          "Toggle a persistent message popup window.
            If popup is visible but unselected, select it.
            If popup is focused, kill it."
          (interactive)
          (if-let ((win (get-buffer-window "*Messages*")))
              (if (eq (selected-window) win)
                  ;; If users attempt to delete the sole ordinary window, silence it.
                  (ignore-errors (kill-buffer "*Messages*"))
                (select-window win))
            (switch-to-buffer (get-buffer "*Messages*"))
            (goto-char (point-max))
            ))

        (global-set-key (kbd "M-`") 'chin/message-toggle)

        (require 'savehist)
        (savehist-mode)

        (setq word-wrap-by-category t)

        (setq kill-whole-line t)

        (when (boundp 'diff-hl-mode)
          (add-hook 'prog-mode-hook 'diff-hl-mode)
          (add-hook 'org-mode-hook 'diff-hl-mode))

        (block! "Region Expand"
                :packages ("expand-region")
                (define-advice set-mark-command (:before-while (arg))
                  "Repeat C-SPC to expand region."
                  (interactive "P")
                  (if (eq last-command 'set-mark-command)
                      (progn
                        (er/expand-region 1)
                        nil)
                    t)))
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

        ;; Toolbar Settings
        (tool-bar-mode -1)

        ;; Mode-line settings
        ;; (add-hook 'after-init-hook #'column-number-mode)
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
        ;; Frame title settings
        (setq frame-title-format "Emacs - %b  %f")
        (setq backup-directory-alist `(("." . "~/.emacs-saves")))


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
        (block! "Point Stack"
                :packages ("point-stack.el")
                ;; Back Or Forward Settings
                (require 'point-stack)
                (point-stack-setup-advices)
                (global-set-key (kbd "M-1") 'point-stack-pop)
                (global-set-key (kbd "M-2") 'point-stack-forward-stack-pop)))

(block! "Eglot"
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
                    (setq eldoc-documentation-strategy #'eldoc-documentation-compose))))

(block! "Project"
        (with-eval-after-load 'project
          (add-to-list 'project-vc-ignores ".ccls-cache/")
          (add-to-list 'project-vc-ignores "node_modules")
          (defvar project-language-aware-root-files
            '("tsconfig.json"
              ".git"
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
                    #'project-try-language-aware)))

(block! "Window and Buffer Management"
        (block! "Window Jump"
                (global-set-key (kbd "C-<right>") 'windmove-right)
                (global-set-key (kbd "C-<left>") 'windmove-left)
                (global-set-key (kbd "C-<up>") 'windmove-up)
                (global-set-key (kbd "C-<down>") 'windmove-down)

                (defun chin/select-window ()
                  (interactive)
                  (let ((normal-window-list)
                        (window-list-size)
                        (selected-window))
                    (setq normal-window-list
                          (seq-filter
                           (apply-partially (lambda (e) (not (window-dedicated-p e))))
                           (window-list)))
                    (setq window-list-size (length normal-window-list))
                    (setq selected-window
                          (if (= window-list-size 1)
                              (car-safe normal-window-list)
                            (let ((ovs nil)
                                  (loop 0)
                                  (ov nil)
                                  (key)
                                  (key-wins nil))
                              (dolist (win normal-window-list)
                                (setq key (nth loop base-sidebar-key-set))
                                (push (list key win) key-wins)
                                (select-window win)
                                (let ((ov (make-overlay (window-start) (1+ (window-start)))))
                                  (push ov ovs)
                                  (overlay-put ov 'face '((:height 3.0 :foreground "#aa0000")))
                                  (overlay-put ov 'display (format "[%s]" key))
                                  (overlay-put ov 'window win))
                                (setq loop (1+ loop)))
                              (setq win-key (ignore-errors (char-to-string (read-char "Please enter key"))))
                              (unless win-key (setq win-key ""))
                              (setq selected-win
                                    (car-safe
                                     (seq-filter (apply-partially (lambda (e) (string= (car-safe e) win-key)))
                                                 key-wins)))
                              (mapcar #'delete-overlay ovs)
                              (car-safe (cdr-safe selected-win)))))
                    (when selected-window (select-window selected-window))
                    selected-window))

                (global-set-key (kbd "M-j") 'chin/select-window))
        )

(block! "File Settings"
        (block! "Dired"
                :packages ("mdired.el")
                (defadvice dired-find-file (around dired-find-file-single-buffer activate)
                  "Replace current buffer if file is a directory."
                  (interactive)
                  (let ((orig (current-buffer)) (filename (dired-get-file-for-visit)))
                    ad-do-it
                    (when (and (file-directory-p filename)
                               (not (eq (current-buffer) orig)))
                      (kill-buffer orig))))

                (block! "Dired-Subtree"
                        :packages ("site-lisp/dired-hack-utils.el" "site-lisp/dired-subtree.el")
                        (defun chin/once-dired-subtree ()
                          (interactive)
                          (let ((file (ignore-errors (file-truename (buffer-file-name))))
                                (iterp t)
                                start parent dir)
                            (project-dired)
                            (dired-hide-details-mode 1)
                            (when file
                              (setq dir (let ((dir (file-truename default-directory)))
                                          (if (string-match ".*/$" dir)
                                              dir
                                            (concat dir "/"))))
                              (setq start (length dir))

                              (goto-char (point-min))
                              (while-let ((index (or (string-search "/" file start)))
                                          (iterp2 t))
                                (setq parent (substring file nil index))
                                (while (and (not (eobp)) iterp2)
                                  (dired-next-line 1)
                                  (when-let* ((fap (dired-file-name-at-point))
                                              (samep (file-equal-p parent fap)))
                                    (dired-subtree-insert)
                                    (setq iterp2 nil)))
                                (setq start (1+ index)))
                              (while (and iterp (not (eobp)))
                                (dired-next-line 1)
                                (when-let* ((fap (thing-at-point 'filename))
                                            (samep (file-equal-p file (expand-file-name fap parent))))
                                  (setq iterp nil))))

                            (keymap-local-set "<tab>" 'dired-subtree-toggle)
                            (keymap-local-set "M-l" 'quit-window)))
                        (global-set-key (kbd "M-l") 'chin/once-dired-subtree)))

        (block! "Recentf"
                :packages ("recentf")
                (require 'recentf)
                (recentf-mode 1)

                (defconst chin/hist-files-file "~/.hist-files")

                (defun chin/is-private-file (filename)
                  (and filename (string-match-p ".*/private/.*" filename)))

                (defun chin/add-visited-file ()
                  (let ((filename (buffer-file-name)))
                    (unless (chin/is-private-file filename)
                      (write-region (concat filename "\n") nil chin/hist-files-file t))))

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
                (add-hook 'find-file-hook 'chin/add-visited-file))
        ;; Misc settings
        ;; Auto save files after losing focus.
        ;; https://emacs.stackexchange.com/questions/60970/how-to-replace-focus-out-hook-with-after-focus-change-function-in-emacs-27
        (add-function :after after-focus-change-function
                      (lambda () (save-some-buffers t)))

        (defun chin/today-file (directory)
          (interactive)
          (let* ((ct (current-time))
                 (sub-dir (format-time-string "%y%m-%d-T%H%M%S.tp" ct))
                 (time (completing-read
                        (concat "Timebased file (" directory "): ")
                        (if (file-exists-p directory)
                            (directory-files-recursively directory "" t (lambda (x) (not (string-match-p  "/\\." x))))
                          nil) nil nil sub-dir))
                 (total (expand-file-name time directory)))
            (mkdir (file-name-parent-directory total) t)
            (find-file total)))

        (global-set-key (kbd "C-<f5>") (lambda () (interactive) (chin/today-file chin/playground-data-dir)))

        )

(block! "Chinese"
        (defun chin/insert-half-width-space-between-chinese-and-english ()
          "在中英文之间增加半角空格"
          (interactive)
          (save-excursion
            (goto-char (point-min))
            (while (re-search-forward "\\([a-zA-Z0-9]\\)\\([^\x00-\xff]\\)" nil t)
              (replace-match "\\1 \\2"))
            (goto-char (point-min))
            (while (re-search-forward "\\([^\x00-\xff]\\)\\([a-zA-Z0-9]\\)" nil t)
              (replace-match "\\1 \\2")))))

(block! "Folding"
        :packages ("site-lisp/yafolding.el")
        (add-hook 'prog-mode-hook 'yafolding-mode))

(block! "utils"
        (defun chin/server-shutdown ()
          "Save buffers, quit, and shutdown (kill) server."
          (interactive)
          (save-some-buffers)
          (kill-emacs)))
(block! "Magit"
        :packages ("magit")

        (setq magit-display-buffer-function
              (lambda (buffer)
                (display-buffer
                 buffer (if (and (derived-mode-p 'magit-mode)
                                 (memq (with-current-buffer buffer major-mode)
                                       '(magit-process-mode
                                         magit-revision-mode
                                         magit-diff-mode
                                         magit-stash-mode
                                         magit-status-mode)))
                            nil
                          '(display-buffer-same-window))))))
