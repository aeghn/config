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

(defun chin/todo (directory)
  (interactive)
  (when-let ((buf (find-file (expand-file-name "todo.org" directory))))
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
(global-set-key (kbd "<f5>") (lambda () (interactive) (chin/todo "~/extra/firm/")))

(defun chin/today-file (directory)
  (interactive)
  (let* ((ct (current-time))
         (today (format-time-string "%Y-%m/%d" ct))
         (dir (expand-file-name today directory))
         (time
          (completing-read (concat "Timebased file (" dir "): ")
                           (if (file-exists-p dir) (directory-files dir) nil)
                           nil
                           nil
                           (format-time-string "t%H%M%S" ct)))
         (ct-file (expand-file-name time dir)))
    (mkdir dir t)
    (find-file ct-file)))

(global-set-key (kbd "C-<f6>") (lambda () (interactive) (chin/today-file "~/extra/tmps")))
(global-set-key (kbd "C-<f5>") (lambda () (interactive) (chin/today-file "~/extra/firm/days/")))


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
