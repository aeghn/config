(defcustom chin/writer-dir (expand-file-name "~/files/docs/org") "")

(defun chin/org-show-current-heading-tidily ()
  (interactive)  ;Inteactive
  "Show next entry, keeping other entries closed."
  (if (save-excursion (end-of-line) (outline-invisible-p))
      (progn (org-show-entry) (show-children))
    (outline-back-to-heading)
    (unless (and (bolp) (org-on-heading-p))
      (org-up-heading-safe)
      (hide-subtree)
      (error "Boundary reached"))
    (org-overview)
    (org-reveal t)
    (org-show-entry)
    (show-children)))

(defun chin/todo (filename)
  (interactive)
  (when-let ((buf (find-file filename)))
    (with-current-buffer buf
      (let* ((today (format-time-string "%Y-%m-%d" (current-time)))
             (pos (org-find-exact-headline-in-buffer today nil t)))
        (if pos
            (progn
              (goto-char pos)
              (next-line))
          (goto-char (point-max))
          (newline)
          (insert "* " today))
        (chin/org-show-current-heading-tidily)))))

(global-set-key (kbd "<f6>") (lambda () (interactive) (chin/todo chin/writer-dir)))

(defun chin/today-file (directory)
  (interactive)
  (let* ((ct (current-time))
         (sub-dir (format-time-string "%Y-%m/%d/t%H%M%S" ct))
         (time (completing-read
                (concat "Timebased file (" directory "): ")
                (if (file-exists-p directory)
                    (directory-files-recursively directory "" t (lambda (x) (not (string-match-p  "/\\." x))))
                    nil) nil nil sub-dir))
         (total (expand-file-name time directory)))
    (mkdir (file-name-parent-directory total) t)
    (find-file total)))

;; (global-set-key (kbd "C-<f6>") (lambda () (interactive) (chin/today-file "~/extra/tmps")))

(defun chin/number-items ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (replace-regexp "\\(^[[:space:]]*\\)（?\\([0-9]+\\)）" "\\1\\2. ")
    (goto-char (point-min))
    (replace-regexp "\\(^[[:space:]]*[0-9]+\\)、" "\\1. ")
    (goto-char (point-min))
    (replace-regexp "\\(^[[:space:]]*[0-9]+\\)，" "\\1. ")
    (goto-char (point-min))
    (replace-regexp "\\(^[[:space:]]*[0-9]+\\))" "\\1. ")
    (goto-char (point-min))
    (replace-regexp "\\(^[[:space:]]*[0-9a-zA-Z]+\.\\)[ ]*" "\\1 ")
    (goto-char (point-min))
    (replace-regexp "\\(^[[:space:]]*\\)\\-+[[:space:]]*" "\\1 - ")))


(defun uijm ()
  (interactive)
  (insert (format-time-string "%y-%m-%d %H:%M:%S")))

(defun uijm-file ()
  (interactive)
  (insert (format-time-string "%y%m%d_%H%M%S")))
