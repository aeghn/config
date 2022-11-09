(require 'dired)
(require 'dired-x)

(defgroup mdired nil
  "Dired with preview and some other features"
  :group 'convenience)

(defvar-local mdired-object-vector nil
  "The variable which saves the infomation")

(defcustom mdired-conponments
  '(filename
    directory-window directory-buffer
    parent-window parent-buffer
    info-window info-buffer
    preview-window preview-buffer
    path-window path-buffer)
  "the infomations we need to save")

(defvar-keymap mdired-mode-map
  "<left>"  'mdired-set-current-parent
  "<right>" 'mdired-set-current-child)

(define-minor-mode mdired-mode
  "mdired"
  :global nil
  :group mdired
  (mdired-set-current-by-dired)
  (mdired-hide-details)
  (add-hook 'post-command-hook
            (lambda () (mdired-set-current-by-dired))
            nil t))

(defun mdired (filename)
  (interactive "f")
  (let ((dired-hide-details-mode t)
        (dired-free-space nil)
        (directory (if (file-directory-p filename)
                       filename
                     (file-name-parent-directory filename))))
    (with-current-buffer (dired directory)
      (mdired-build-getter-and-setters)
      (mdired-find-file filename))))

(defun mdired-hide-details (&optional show-all)
  "Hide unneed infomation in this dired buffer, such as file infomations
and dired header lines."
  (if show-all
      (revert-buffer)
    (let ((dired-hide-details-mode t)
          (dired-free-space nil))
      (mdired-hide-header)
      (dired-hide-details-update-invisibility-spec))))

(defun mdired-hide-header ()
  "Delete the header line and the `.' and the `..'"
  (let ((buffer-read-only nil))
    (save-excursion
      (goto-char (point-min))
      (while (or (string-match-p
                  (concat "^  "
                          (regexp-quote
                           (directory-file-name
                            (expand-file-name default-directory)))
                          ":.*")
                  (buffer-substring-no-properties
                   (line-beginning-position)
                   (line-end-position)))
                 (string-match-p
                  "^\\.\\.?"
                  (if-let* ((begin (dired-move-to-filename nil))
                            (end (dired-move-to-end-of-filename t))
                            (str (buffer-substring-no-properties begin end)))
                      str "")))
        (delete-region (line-beginning-position) (1+ (line-end-position)))))))

(defun mdired-find-file (filename)
  "Find the file in the current buffer.

If FILENAME is a directory, just jump into it,
else we will jump into its parent and goto this file."
  (interactive)
  (let ((dired-hide-details-mode t)
        (dired-free-space nil)
        (old-vector mdired-object-vector))
    ;; If the file is a directory, so we just jump into it,
    ;; else we get into its parent directory and goto this file.
    (if (directory-name-p filename)
        (find-alternate-file filename)
      (let ((directory (file-name-parent-directory filename))
            ;; FIXME: does this work for soft links?
            (file (expand-file-name filename)))
        (unless (string= directory default-directory)
          (find-alternate-file directory))
        (unless (string= (file-name-nondirectory filename)
                         (dired-get-filename t t))
          (dired-goto-file file))))
    ;; Todo: is there existing any better method for us to set this vector?
    (setq-local mdired-object-vector old-vector)
    (mdired-mode)))

(defun mdired-set-current-by-dired ()
  "Set current file by using `mdired--get-dired-filename'"
  (mdired-set-current
   (mdired--get-dired-filename)))

(defun mdired--get-dired-filename ()
  "Get filename by using `dired-get-filename'.

If it is on the line which has no file, just return
current directory with a slash in the end."
  (if-let ((filename (dired-get-filename nil t)))
      filename
    (file-name-as-directory default-directory)))

;; Helper Functions.
(defun mdired--remove-slash-more (filename)
  "Remove the current directory from the path."
  (if (directory-name-p filename)
      (directory-file-name filename)
    (directory-file-name (file-name-parent-directory filename))))

(defun mdired--is-root-dir (filename)
  "Check if we arraived on the root dir."
  (not (file-name-parent-directory filename)))

(defun mdired-set-current-parent ()
  "Go to the parent dir when we don't on the root dir."
  (interactive)
  (let ((parent (mdired--remove-slash-more
                 (mdired--get-filename))))
    (unless (mdired--is-root-dir parent)
      ;; check if we already arrived on the root dir, so do not
      ;; disturb it.
      (mdired-set-current parent t))))

(defun mdired-set-current-child ()
  "Goto the selection file/directory."
  (interactive)
  (let ((filename (mdired--get-filename)))
    (mdired-set-current
     (if (file-directory-p filename)
         (file-name-as-directory filename)
       filename)
     t)))

(defun mdired-set-current (filename &optional refresh)
  "Set dired current file to FILENAME if it exists
and is different from current file."
  (when-let (filename
             (absolute-filename (expand-file-name filename))
             (should-set (or (null (mdired--get-filename))
                             (not (string= absolute-filename
                                           (mdired--get-filename)))))
             (file-exists (file-exists-p absolute-filename)))
    (mdired--set-filename absolute-filename)
    (when refresh (mdired-refresh))))

(defun mdired-refresh ()
  "Refresh all mdired windows."
  (mdired-find-file (mdired--get-filename)))

(defun mdired-build-getter-and-setters ()
  "Build getter and setters"
  (let ((index 0)
        (length (length mdired-conponments)))
    (setq-local mdired-object-vector (make-vector length nil))
    (dolist (e mdired-conponments)
      (defalias (intern (format "mdired--set-%s" e))
        `(lambda (newval)
           (interactive)
           (aset mdired-object-vector ,index newval)))
      (defalias (intern (format "mdired--get-%s" e))
        `(lambda ()
           (interactive)
           (aref mdired-object-vector ,index)))
      (defalias (intern (format "mdired--null-%s" e))
        `(lambda ()
           (interactive)
           (null (aref mdired-object-vector ,index))))
      (setq index (1+ index)))))
