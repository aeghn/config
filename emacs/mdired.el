(require 'dired)
(require 'dired-x)

(defgroup mdired nil
  "Dired with preview and some other features"
  :group 'convenience)

(defvar mdired-mode-toolbar-map nil
  "The variable which holds the toolbar buttons")

(defvar-local mdired-object-vector nil
  "The variable which saves the infomation")

(defvar-local mdired-main-buffer nil)
(defvar-local mdired--hl-overlay nil)

(defcustom mdired-conponments
  '(filename
    directory-window directory-buffer
    parent-window parent-buffer
    info-window info-buffer
    preview-window preview-buffer
    path-window path-buffer)
  "the infomations we need to save")

(defun mdired (filename)
  "Open mdired operations"
  (interactive "f")
  (let ((dired-hide-details-mode t)
        (dired-free-space nil)
        (directory (if (file-directory-p filename)
                       filename
                     (file-name-parent-directory filename))))
    (with-current-buffer (dired directory)
      (mdired-build-getter-and-setters)
      (mdired--set-filename mdired-object-vector (dired-get-filename))
      (mdired-refresh mdired-object-vector))))

(defun mdired-refresh (vector &optional exclude-main)
  "Refresh this mdired instance."
  (unless exclude-main
    (mdired-find-file (mdired--get-filename vector)))
  (mdired--refresh-path vector)
  (mdired--refresh-parent vector))

(defun mdired-set-current (filename)
  "Set dired current file to FILENAME if it exists
and is different from current file."
  (when-let (filename
             (absolute-filename (expand-file-name filename))
             (should-set (not (string= absolute-filename
                                       (mdired--get-filename mdired-object-vector))))
             (file-exists (file-exists-p absolute-filename)))
    (mdired--set-filename mdired-object-vector absolute-filename)))

;;; Getter and Setters
(defun mdired-build-getter-and-setters ()
  "Build getter and setters"
  (let ((index 0)
        (length (length mdired-conponments)))
    (setq-local mdired-object-vector (make-vector length nil))
    (dolist (e mdired-conponments)
      (defalias (intern (format "mdired--set-%s" e))
        `(lambda (vector newval)
           (interactive)
           (aset vector ,index newval)
           newval))
      (defalias (intern (format "mdired--get-%s" e))
        `(lambda (vector)
           (interactive)
           (aref vector ,index)))
      (defalias (intern (format "mdired--null-%s" e))
        `(lambda (vector)
           (interactive)
           (null (aref vector ,index))))
      (setq index (1+ index)))))

;;; Helper Functions
(defun mdired--get-dired-filename ()
  "Get filename by using `dired-get-filename'.

If it is on the line which has no file, just return
current directory with a slash in the end."
  (if-let ((filename (dired-get-filename nil t)))
      filename
    (file-name-as-directory default-directory)))

(defun mdired--remove-slash-more (filename)
  "Remove the current directory from the path.

Maybe nil."
  (if-let ((filename)
           (parent (file-name-parent-directory filename)))
      (if (directory-name-p filename)
          (directory-file-name filename)
        (directory-file-name parent))
    nil))

(defun mdired--is-root-dir (filename)
  "Check if we arraived on the root dir."
  (not (file-name-parent-directory filename)))

(defun mdired--check-live-window (window)
  (when (window-live-p window) window))

;;; Dired Window Functions
(defvar-keymap mdired-mode-map
  "<left>"  'mdired-set-current-parent
  "<right>" 'mdired-set-current-child)

(define-minor-mode mdired-mode
  "mdired -- just for the main dired window's minor mode"
  :lighter " mdired "
  :global nil
  :group mdired
  (mdired-set-current-by-dired)
  (mdired-hide-details)
  (add-hook 'post-command-hook
            (lambda ()
              (when (bound-and-true-p mdired-object-vector)
                (mdired-set-current-by-dired)))
            nil t))

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
    ;; Todo: is there any better method for us to set this vector?
    (setq-local mdired-object-vector old-vector)
    (mdired--set-toolbar-buttons)
    (mdired--set-directory-buffer mdired-object-vector (current-buffer))
    (mdired--set-directory-window mdired-object-vector (selected-window))
    (mdired-mode)))

(defun mdired-set-current-by-dired ()
  "Set current file by using `mdired--get-dired-filename'"
  (mdired-set-current (mdired--get-dired-filename))
  ;; Highlight current line
  (if-let ((overlay mdired--hl-overlay))
      (move-overlay overlay (line-beginning-position) (1+ (line-end-position)))
    (setq-local mdired--hl-overlay
                (make-overlay (line-beginning-position) (1+ (line-end-position))))
    (overlay-put mdired--hl-overlay 'face '((:inherit highlight :extend t))))
  (mdired-refresh mdired-object-vector t))

(defun mdired-set-current-parent ()
  "Go to the parent dir when we don't on the root dir."
  (interactive)
  ;; Because we can only use this function in the main dired window, so
  ;; mdirediobject-vector can be called quickly.
  (let ((parent (mdired--remove-slash-more
                 (mdired--get-filename mdired-object-vector))))
    (unless (mdired--is-root-dir parent)
      ;; check if we already arrived on the root dir, so do not
      ;; disturb it.
      (mdired-set-current parent)
      (mdired-refresh mdired-object-vector))))

(defun mdired-set-current-child ()
  "Goto the selection file/directory, same as `mdired-set-current-parent',
but for child."
  (interactive)
  (let ((filename (mdired--get-filename mdired-object-vector)))
    (mdired-set-current
     (if (file-directory-p filename)
         (file-name-as-directory filename)
       filename))
    (mdired-refresh mdired-object-vector)))

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

;;; Path window functions
(defun mdired--get-or-build-path-window (vector)
  (let ((window (mdired--get-path-window vector))
        path-window)
    (if (mdired--check-live-window window)
        window
      (mdired--set-directory-window
       vector
       (split-window-vertically 2 (mdired--get-directory-window vector)))
      (setq path-window (mdired--set-path-window vector (selected-window)))
      (select-window (mdired--get-directory-window vector))
      path-window)))

(defun mdired--get-or-build-path-buffer (vector)
  (if-let* ((buffer (mdired--get-path-buffer vector))
            (buffer-live (buffer-live-p buffer)))
      buffer
    (mdired--set-path-buffer
     vector
     (let ((buffer (generate-new-buffer "mdired-path-buffer")))
       ;; we don't want any modeline in the path window
       (with-current-buffer buffer
         (setq-local mode-line-format nil
                     buffer-read-only t
                     truncate-lines t)
         (mdired--set-toolbar-buttons))
       buffer))))

(defun mdired-jump-by-path-button (button)
  (interactive)
  (when-let ((main-buffer (button-get button 'main-buffer))
             (path (button-get button 'token)))
    (with-current-buffer main-buffer
      (when-let ((vector mdired-object-vector)
                 (main-window (mdired--get-directory-window vector)))
        (select-window main-window)
        (mdired--set-filename vector path)
        (mdired-refresh vector)))))

(defun mdired-toggle-parent (&rest args)
  (interactive)
  (when-let* ((vector (or (bound-and-true-p mdired-object-vector)
                          (with-current-buffer mdired-main-buffer
                            mdired-object-vector))))
    (if-let ((window (mdired--check-live-window (mdired--get-parent-window vector))))
        (delete-window window)
      (mdired--refresh-parent vector t))))

(defun mdired--make-other-path-buttons (vector)
  (let ((button-list '(;; lighter function echo
                       ("" mdired-toggle-parent  "Toggle Parent Window" mdired--get-parent-window)
                       ("" mdired-toggle-preview "Toggle Preview Window" mdired--get-preview-window)
                       ("" mdired-toggle-info    "Toggle Info Window" mdired--get-info-window)))
        button-start underline)
    (dolist (p button-list)
      (insert " ")
      (setq button-start (point))
      (insert (nth 0 p))
      (setq underline (if (mdired--check-live-window (funcall (nth 3 p) vector)) t nil))
      (make-button button-start (point)
                   'action (nth 1 p)
                   'follow-link t
                   'face `((:underline ,underline))
                   'help-echo (nth 2 p)))))

(defun mdired--refresh-path (vector)
  (with-selected-window (mdired--get-or-build-path-window vector)
    (switch-to-buffer (mdired--get-or-build-path-buffer vector))
    (with-current-buffer (mdired--get-or-build-path-buffer vector)
      (setq-local mdired-main-buffer
                  (mdired--get-directory-buffer vector))
      (let ((buffer-read-only nil)
            begin bs be path end)
        (erase-buffer)
        (setq bs (point)
              begin (point))
        (insert (mdired--get-filename vector))
        (setq end (point))
        (goto-char begin)
        ;; FIXME: we assume there is only one path divider "/"
        (while (search-forward "/" end t)
          (setq be (1- (point))
                path (buffer-substring begin (point)))
          (make-button bs be
                       'action 'mdired-jump-by-path-button
                       'follow-link t
                       'face '((:underline t))
                       'token path
                       'main-buffer mdired-main-buffer
                       'help-echo path)
          (setq bs (point)))
        (goto-char (line-end-position))
        ;; (mdired--make-other-path-buttons vector)
        )
      (goto-char (point-max)))))

;;; Parent Window Functions
(defun mdired--get-or-build-parent-window (vector &optional build)
  (let ((window (mdired--get-parent-window vector))
        parent-window)
    (if (mdired--check-live-window window)
        window
      (when build
        (select-window (mdired--get-directory-window vector))
        (mdired--set-directory-window
         vector
         (split-window-horizontally 35 (mdired--get-directory-window vector)))
        (setq parent-window (mdired--set-parent-window vector (selected-window)))
        (select-window (mdired--get-directory-window vector))
        parent-window))))

(defun mdired--get-or-build-parent-buffer (vector)
  (or (mdired--get-parent-buffer vector)
      (mdired--set-parent-buffer
       vector
       (let ((buffer (generate-new-buffer "mdired-parent-buffer")))
         (with-current-buffer buffer
           (setq mode-line-format "Mdired Parent"
                 buffer-read-only t)
           (mdired--set-toolbar-buttons))
         buffer))))

(defun mdired--refresh-parent (vector &optional build)
  (when-let ((dired-window (mdired--check-live-window
                            (mdired--get-directory-window vector)))
             (parent-window (mdired--get-or-build-parent-window vector build))
             (parent-buffer (mdired--get-or-build-parent-buffer vector)))
    (with-selected-window parent-window
      (switch-to-buffer parent-buffer))
    (with-current-buffer parent-buffer
      (setq-local mdired-main-buffer (mdired--get-directory-buffer vector))
      (let* ((item-path (mdired--remove-slash-more (mdired--get-filename vector)))
             (parent-path (mdired--remove-slash-more item-path))
             (buffer-read-only nil))
        (unless (string= item-path default-directory)
          (unless (string= parent-path (mdired--remove-slash-more default-directory))
            (erase-buffer)
            (when parent-path
              (process-file "ls" nil t nil "-A" "-p" (mdired--remove-slash-more item-path)))
            ;; highlight current line
            (let ((itemname (file-name-nondirectory item-path)))
              (goto-char (point-min))
              (when-let* ((end (re-search-forward (concat "^" (regexp-quote (file-name-as-directory itemname)) "$") nil t))
                          (overlay (make-overlay (line-beginning-position) (1+ (line-end-position)))))
                ;; TODO: remove unused overlays
                (overlay-put overlay 'face '((:inherit highlight :extend t)))))
            (setq-local default-directory item-path)))))))


;;; Tool Bar Related Functions
(defun mdired--make-toolbar-buttons ()
  (unless mdired-mode-toolbar-map
    (add-to-list
     'image-load-path
     (expand-file-name "lib/images" user-emacs-directory))
    (setq mdired-mode-toolbar-map
          (let ((map (make-sparse-keymap)))
                        (define-key map [mdired-toggle-info]
                        `(menu-item "Info" mdired-toggle-info
                                    :enable t
                                    :help "Toggle Info Window"
                                    :image ,(find-image '((:type svg :file "emacs-info.svg")))))                                    
            (define-key map [mdired-toggle-preview]
                        `(menu-item "Preview" mdired-toggle-preview
                                    :enable t
                                    :help "Toggle Preview Window"
                                    :image ,(find-image '((:type svg :file "emacs-mdired-preview-button.svg")))))
            (define-key map [mdired-toggle-parent]
                        `(menu-item "Parent" mdired-toggle-parent
                                    :enable t
                                    :help "Toggle Parent Window"
                                    :image ,(find-image '((:type svg :file "emacs-mdired-parent-button.svg")))))
            map)))
  mdired-mode-toolbar-map)

(defun mdired--set-toolbar-buttons ()
  (setq-local tool-bar-map (mdired--make-toolbar-buttons)))
