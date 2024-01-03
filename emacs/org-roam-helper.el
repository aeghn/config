(require 'org-roam)
(require 'consult)

(defun org-roam-helper-get-org-file-title ()
  "Get the title of the current org-mode file."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^#\\+title:\\s-*\\(.*\\)$" nil t)
      (substring-no-properties (match-string 1)))))

(defun org-roam-helper-get-headlines-ancentors ()
  (let ((level 9999)
        cur-lvl heading-list)
    (save-excursion
      (while (re-search-backward "^\\(\\*+\\)\\s-*\\(.*\\)$" nil t)
        (setq cur-lvl (length (substring-no-properties (match-string 1))))
        (when (< cur-lvl level)
          (setq level cur-lvl)
          (push (substring-no-properties (match-string 2)) heading-list))))
    heading-list))

(defun org-roam-helper-get-roam-alias ()
  (let ((title (org-roam-helper-get-org-file-title))
        (lines  (org-roam-helper-get-headlines-ancentors)))
    (cond ((and title lines) (string-replace "_" " " (concat title "/" (string-join lines "/"))))
          (title title)
          (lines (string-replace "_" " " (string-join lines "/"))))))

(defun org-roam-helper-turn-to-node ()
  (interactive)
  (when-let ((title (org-roam-helper-get-roam-alias))
             (id "")
             (p (point)))
    (setq id (org-id-get-create))
    (unless (org-entry-get p "ROAM_ALIASES")
      (org-roam-alias-add title))
    `(,id ,title)))

(defun org-roam-helper-grep ()
  (interactive)
  (consult-ripgrep org-roam-directory))

(defun org-roam-helper-grep-and-insert-node ()
  (interactive)
  (when-let ((buffer (current-buffer))
             (id-title (save-excursion
                         (org-roam-helper-grep)
                         (unless (eq (current-buffer) buffer)
                           (goto-char (line-end-position))
                           (when-let* ((id-title (org-roam-helper-turn-to-node)))
                             (save-buffer)
                             id-title)))))
    (switch-to-buffer buffer)
    (insert (format "[[id:%s][%s]]" (car id-title) (cadr id-title)))))

(defun org-roam-helper-open-at-point (&optional other-window)
  (interactive)
  (if other-window
      (let ((org-link-frame-setup (quote
                                   ((vm . vm-visit-folder)
                                    (vm-imap . vm-visit-imap-folder)
                                    (gnus . gnus)
                                    (file . find-file)
                                    (wl . wl)))
                                  ))
        (org-open-at-point))
    (org-open-at-point)))

