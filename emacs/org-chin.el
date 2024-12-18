;;; -*- lexical-binding: t; -*-

;; Org-mode settings
(require 'org)
(require 'org-tempo)
;; (require 'org-tidy)
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
  (corfu-mode -1)
  (setq truncate-lines nil)
  (when chin/is-windows
    (define-key org-mode-map (kbd "C-y") 'chin/org-paste))
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
  (let* ((date (format-time-string "%y%m-%d"))
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

(defun chin/org-paste ()
  (interactive)
  (let* ((output (shell-command-to-string "clip2file -d ./assets/"))
         (lines (split-string output "\n")))
    (if (string= (string-trim output) "NOT_IMG_NOR_FILE")
        (yank)
      (dolist (line lines)
        (unless (string-empty-p line)
          (insert "[[" line "]]"))))))
