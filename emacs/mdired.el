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

(defcustom mdired-listing-switches
  '(("-alh"  . "name-asc")
    ("-alhr" . "name-desc")
    ("-alhXr"  . "ext-asc")
    ("-alhX" . "ext-desc")
    ("-alhSr"  . "size-asc")
    ("-alhS" . "size-desc")
    ("-alhtr"  . "time-asc")
    ("-alht" . "time-desc"))
  "")

(defconst mdired-extensions
  '((audio "mp3" "aac" "ogg" "flac" "alac" "wav" "aiff" "dsd" "pcm")
    (video "mp4" "mov" "wmv" "avi" "avchd" "flv" "f4v" "swf" "mkv" "webm" "mpeg")
    (image "apng" "avif" "gif" "jpg" "jpeg" "jfif" "pjpeg" "pjp" "png" "svg" "webp" "bmp" "ico" "cur" "tif" "tiff")
    (code "el" "js" "ts" "rs" "c" "cc" "cpp" "h" "hh" "hpp" "java" "sh" "bash" "zsh" "css" "py" "pl" "lisp" "conf" "inf" "yml" "yaml" "xml" "json")
    (webpage "html" "htm")
    (ebook "pdf" "epub" "mobi"))
  "")

(defconst mdired-font-icons
  '((ebook . "")
    (rich-text . "")
    (book . "")
    (code . "")
    (file . "")
    (database . "")
    (config . "")
    (compress . "")
    (folder . "")
    (image . "")
    (mindmap . "")
    (video . "")
    (audio . "")
    (secret . ""))
  "")

(defvar mdired--extension-icons nil
  "")

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
    (setq-default dired-listing-switches (car (car mdired-listing-switches)))
    (with-current-buffer (dired directory )
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

(defun mdired--try-get-vector ()
  (cond (mdired-object-vector mdired-object-vector)
        (mdired-main-buffer (with-current-buffer mdired-main-buffer
                              mdired-object-vector))
        (t nil)))

(defun mdired--try-get-dired-buffer ()
  (cond (mdired-object-vector (current-buffer))
        (mdired-main-buffer mdired-main-buffer)
        (t nil)))

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
  (mdired-dired-details (not dired-hide-details-mode))
  (mdired--dired-refresh-icons)
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
        (old-buffer (current-buffer))
        (old-vector mdired-object-vector)
        (old-listing-switches dired-listing-switches)
        (old-hide-details-mode dired-hide-details-mode))
    ;; If the file is a directory, so we just jump into it,
    ;; else we get into its parent directory and goto this file.
    (setq-default dired-listing-switches old-listing-switches)
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
    (setq-local dired-hide-details-mode old-hide-details-mode)
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

(defun mdired-dired-details (&optional show-detail)
  "Hide unneed infomation in this dired buffer, such as file infomations
and dired header lines."
  (let ((dired-free-space nil))
    (mdired-hide-header)
    (if show-detail
        (setq-local dired-hide-details-mode nil)
      (setq-local dired-hide-details-mode t))
    (dired-hide-details-update-invisibility-spec)))

(defun mdired-hide-header ()
  "Hide the header line and remove the `.' and the `..'"
  (let ((buffer-read-only nil))
    (save-excursion
      (goto-char (point-min))
      ;; Hide the first header line
      (when-let* ((regex-dir (regexp-quote
                              (directory-file-name
                               (expand-file-name default-directory))))
                  (line (buffer-substring-no-properties
                         (line-beginning-position)
                         (line-end-position)))
                  (match (string-match
                          (concat "^  " regex-dir ":\\(.*\\)") line))
                  (mstr (match-string 1)))
        (put-text-property (line-beginning-position) (1+ (line-end-position)) 'display mstr)
        (forward-line))

      (while (not (eobp))
        (if-let* ((begin (dired-move-to-filename nil))
                  (end (dired-move-to-end-of-filename t))
                  (str (buffer-substring-no-properties begin end))
                  (match (string-match-p "^\\.\\.?$" str)))
            (delete-region (line-beginning-position) (1+ (line-end-position)))
          (forward-line))))))

(defun mdired-toggle-detail ()
  (interactive)
  (if-let ((buffer (mdired--try-get-dired-buffer)))
      (with-current-buffer buffer
        (setq-local dired-hide-details-mode (not dired-hide-details-mode))
        (dired-hide-details-mode
         (if dired-hide-details-mode 1 -1)))
    (message "Unable to toggle dired detail")))

(defun mdired-toggle-sort ()
  (interactive)
  (if-let ((buffer (mdired--try-get-dired-buffer)))
      (with-current-buffer buffer
        (let ((old-switches dired-listing-switches)
              (length (length mdired-listing-switches))
              (counter 0)
              (next-index 0))
          (dolist (e mdired-listing-switches)
            (setq counter (1+ counter))
            (when (string= old-switches (car e))
              (setq next-index (% counter length))))
          (let* ((tuple (nth next-index mdired-listing-switches))
                 (switches (car tuple))
                 (name (cdr tuple)))
            (setq-default dired-listing-switches switches)
            (dired-sort-other dired-listing-switches)
            (mdired-hide-header)
            (mdired--set-toolbar-buttons t)
            (message "toggle to %s" name))))
    (message "Unable to toggle dired sort method")))

(defun mdired--get-sort-name ()
  (if-let* ((buffer (mdired--try-get-dired-buffer))
            (result (with-current-buffer buffer
                      (seq-filter (apply-partially
                                   (lambda (x) (string= dired-listing-switches (car x))))
                                  mdired-listing-switches))))
      (cdr-safe (car-safe result))
    (cdr (car mdired-listing-switches))))


;;; Path window functions
(defun mdired--get-or-build-path-window (vector)
  (let ((window (mdired--get-path-window vector))
        path-window)
    (if (mdired--check-live-window window)
        window
      (mdired--set-directory-window
       vector
       (split-window-vertically 2 (mdired--get-directory-window vector)))
      ;; So the window won't change its height
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
         (setq-local window-safe-min-height 0 ; We just need one line to display
                     window-min-height 0
                     mode-line-format nil
                     buffer-read-only t
                     truncate-lines t)
         (setq window-size-fixed 'height)
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
            (mdired--dired-refresh-icons)
            ;; highlight current line
            (let ((itemname (file-name-nondirectory item-path)))
              (goto-char (point-min))
              (when-let* ((end (re-search-forward (concat "^" (regexp-quote (file-name-as-directory itemname)) "$") nil t))
                          (overlay (make-overlay (line-beginning-position) (1+ (line-end-position)))))
                ;; TODO: remove unused overlays
                (overlay-put overlay 'face '((:inherit highlight :extend t)))))
            (setq-local default-directory item-path)))))))


;;; Tool Bar Related Functions
(defun mdired--make-toolbar-buttons (&optional force)
  (when (or force (null mdired-mode-toolbar-map))
    (add-to-list
     'image-load-path
     (expand-file-name "lib/images" user-emacs-directory))
    (let ((map (make-sparse-keymap)))
      (let* ((sort-name (mdired--get-sort-name))
             (icon-file (concat "dired-sort-" sort-name ".svg")))
        (define-key map [mdired-toggle-sort]
                    `(menu-item "Switches" mdired-toggle-sort
                                :enable t
                                :help "Toggle Dired Listing Switches"
                                :image ,(find-image `((:type svg :file ,icon-file))))))
      (define-key map [mdired-show-detail]
                  `(menu-item "Detail" mdired-toggle-detail
                              :enable t
                              :help "Toggle Dired Detail"
                              :image ,(find-image '((:type svg :file "dired-show-detail.svg")))))
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
      (setq mdired-mode-toolbar-map map)))
  mdired-mode-toolbar-map)

(defun mdired--set-toolbar-buttons (&optional force)
  (setq-local tool-bar-map (mdired--make-toolbar-buttons force)))


;;; Icon part functions for dired and parent windows
(defun mdired--dired-refresh-icons ()
  ""
  (mdired--build-font-icons)
  (save-excursion
    (goto-char (point-min))
    (let ((common-icon (concat " " (cdr (assq 'file mdired-font-icons))))
          (folder-icon (concat " " (cdr (assq 'folder mdired-font-icons))))
          (not-dired-mode (not (equal 'dired-mode major-mode)))
          (put-icon (make-symbol "mdired--put-file-icon")))
      (mapc #'delete-overlay
            (seq-filter (apply-partially
                         (lambda (ov)
                           (overlay-get ov 'mdired--icon-overlay)))
                        (overlays-in (point-min) (point-max))))
      (fset put-icon
            (lambda (pos icon)
              (let ((ov (make-overlay pos pos)))
                (overlay-put ov 'mdired--icon-overlay t)
                (overlay-put ov 'after-string icon))))
      (if not-dired-mode
          (while (not (eobp))
            (if (re-search-forward "/" (line-end-position) t)
                (funcall put-icon (line-beginning-position) (concat folder-icon " "))
              (let* ((match (re-search-forward ".*\\.\\([^.]+\\)" (line-end-position) t))
                     (ext (match-string 1))
                     (icon (or (gethash ext mdired--extension-icons) common-icon)))
                (funcall put-icon (line-beginning-position) (concat icon " "))))
            (forward-line))
        (while (not (eobp))
          (when-let* ((match (re-search-forward "..\\(.\\)[rwxt-]\\{9\\}.*\\([ .][^.]+\\)" (line-end-position) t))
                      (type (match-string 1))
                      (ext  (match-string 2))
                      (icon (cond ((string= type "d") folder-icon)
                                  (t (or (gethash ext mdired--extension-icons) common-icon))))
                      (pos (dired-move-to-filename nil)))
            (funcall put-icon (1- pos) icon))
          (forward-line))))))


(defun mdired--build-font-icons (&optional force)
  ""
  (when (or force (null mdired--extension-icons))
    (let ((table (make-hash-table :test 'equal)))
      (dolist (e mdired-extensions)
        (when-let* ((type (car e))
                    (exts (cdr e))
                    (icon (cdr (assq type mdired-font-icons))))
          (dolist (ext exts)
            (puthash (concat "." (downcase ext)) (concat " " icon) table))))
      (setq mdired--extension-icons table))))
