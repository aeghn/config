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
