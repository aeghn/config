;;; org-datamanager.el --- Manage Data With orgmode -*- lexical-binding: t; -*-

(defvar-local last-post-command-line 0
  "Holds the cursor position from the last run of post-command-hooks.")

(defvar-local org-datamanger-current-item-id nil
  "")

(defun org-datamanagr--open-in-right (filepath uuid &optional go-back)
  (when (and filepath uuid)
    (let* ((item (org-datamanager--item-orgpath filepath uuid))
           (item-buffer (get-buffer (car item)))
           (item-path (cdr item))
           (current-window (selected-window))
           (right-win (window-right current-window)))
      (unless right-win (split-window-right))
      (setq right-win (window-right current-window))
      (setq org-datamanger-current-item-id uuid)
      (when right-win
        (with-selected-window right-win
          (if item-buffer
              (switch-to-buffer item-buffer)
            (find-file item-path)
            (org-datamanger-item-mode)))
        (if go-back
            (select-window current-window)
          (select-window right-win))))))

(defun org-datamanager--hook-move-point ()
  (interactive)
  (unless (equal last-post-command-line (line-number-at-pos))
    (setq-local last-post-command-line (line-number-at-pos))
    (let ((id (org-id-get)))
      (unless (and (not (null id))
                   (equal id org-datamanger-current-item-id))
        (org-datamanagr--open-in-right (buffer-file-name) id t)))))

(defun org-datamanger-create-and-open ()
  (interactive)
  (let ((id (org-id-get-create))
        (title (org-get-heading t t t t)))
    (org-datamanagr--open-in-right (buffer-file-name) id)
    (when (< (point-max) 3)
      (goto-char (point-min))
      (insert "#+TITLE: " title "\n\n"))))

(defun org-datamanager--data-dir (filepath &optional create)
  ""
  (let ((path (concat (file-name-sans-extension filepath) "-dmg")))
    (when (and create (not (file-exists-p path))) (mkdir path t))
    path))

(defun org-datamanager--item-orgpath (filepath uuid)
  (let* ((file-subpath-0 (substring uuid 0 1))
         (file-subpath-1 (concat (string-replace "-" "" (substring uuid 1)) ".org"))
         (dirpath (org-datamanager--data-dir filepath))
         (full-path (expand-file-name
                     file-subpath-1
                     (expand-file-name file-subpath-0 dirpath))))
    (cons file-subpath-1 full-path)))


(defun org-datamanger-paste ()
  "Capture the image from the clipboard and insert the resulting file."
  (interactive)
  (let ((org-download-screenshot-method
         (cl-case system-type
           (gnu/linux
            (if (string= "wayland" (getenv "XDG_SESSION_TYPE"))
                (org-datamanager-paste-wayland)
              (if (executable-find "xclip")
                  "xclip -selection clipboard -t image/png -o > %s"
                (user-error
                 "Please install the \"xclip\" program"))))
           ((windows-nt cygwin)
            (if (executable-find "convert")
                "convert clipboard: %s"
              (user-error
               "Please install the \"convert\" program included in ImageMagick")))
           ((darwin berkeley-unix)
            (if (executable-find "pngpaste")
                "pngpaste %s"
              (user-error
               "Please install the \"pngpaste\" program from Homebrew."))))))))

(defun org-datamanager-paste-wayland ()
  (interactive)
  (let* ((filename (concat (file-name-sans-extension
                            (file-name-nondirectory (buffer-file-name)))
                           "-"
                           (format-time-string "%y%m%d-%H%M%S")
                           ".png"))
         (directory (file-name-directory (buffer-file-name)))
         (fullpath (expand-file-name filename directory)))
    (if (string-search "image/png" (shell-command-to-string "wl-paste -l"))
        (progn 
          (shell-command (format "wl-paste -t image/png > %s" fullpath))
          (insert "\n"
                  (format "#+DOWNLOADED_AT: %s" (format-time-string "%y-%m-%d-%H:%M:%S"))
                  "\n"
                  (format "#+attr_org: :width %dpx" 600)
                  "\n"
                  (format "[[file:./%s]]" filename)
                  "\n")
          (org-display-inline-images))
      (org-yank))))

;;;###autoload
(define-minor-mode org-datamanger-mode
  "."
  :global nil
  :group 'org-datamanger
  (setq org-datamanger-current-item-id nil)
  (if org-datamanger-mode
      (add-hook 'post-command-hook #'org-datamanager--hook-move-point nil t)
    (remove-hook 'post-command-hook #'org-datamanager--hook-move-point t)))

;;;###autoload
(define-minor-mode org-datamanger-item-mode
  "."
  :global nil
  :group 'org-datamanger
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-y") 'org-datamanger-paste)
            map)
  (setq-local org-image-actual-width nil)
  (org-display-inline-images))

(provide 'org-datamanger)
