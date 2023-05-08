;;; mdired.el -*- lexical-binding: t; -*-

(require 'dired)
(require 'dired-x)

(defgroup mdired nil
  "Dired with preview and some other features"
  :group 'convenience)

(defcustom mdired-preview-font-text
  "ABCDEFGHIJKLM\nNOPQRSTUVWXYZ\nabcdefghijklm\nnopqrstuvwxyz\n1234567890\n!@#$%^&*(){}[]"
  "Text used to preview font file")

(defcustom mdired-preview-text-threshold 10000000
  "")

(defcustom mdired-listing-switches
  '(("-alh"    . "name-asc")
    ("-alhr"   . "name-desc")
    ("-alhXr"  . "ext-asc")
    ("-alhX"   . "ext-desc")
    ("-alhSr"  . "size-asc")
    ("-alhS"   . "size-desc")
    ("-alhtr"  . "time-asc")
    ("-alht"   . "time-desc"))
  "")

(defcustom mdired--vector-conponments
  '(filename
    dired-window dired-buffer
    parent-window parent-buffer
    info-window info-buffer
    preview-window preview-buffer
    path-window path-buffer)
  "the infomations we need to save")

(defcustom mdired-preview-cache-directory "/tmp/mdired"
  "The directory which we put the cache files")

(defconst mdired-extensions
  '((audio "mp3" "aac" "ogg" "flac" "alac" "wav" "aiff" "dsd" "pcm")
    (video "mp4" "mov" "wmv" "avi" "avchd" "flv" "f4v" "swf" "mkv" "webm" "mpeg")
    (image "apng" "avif" "gif" "jpg" "jpeg" "jfif" "pjpeg" "pjp"
           "png" "svg" "webp" "bmp" "ico" "cur" "tif" "tiff")
    (code "el" "js" "ts" "rs" "c" "cc" "cpp" "h" "hh" "hpp" "java" "sh" "bash" "zsh" "css" "py" "pl"
          "lisp" "conf" "inf" "yml" "yaml" "xml" "json")
    (webpage "html" "htm")
    (ebook "pdf" "epub" "mobi")
    (font  "ttf" "ttc" "otf" "otc")
    (compress "zip" "7z" "tar" "gz" "xz" "rar" "zst")
    (iso "iso"))
  "")

(defconst mdired-font-icons
  '((ebook     . "")
    (rich-text . "")
    (book      . "")
    (code      . "")
    (file      . "")
    (database  . "")
    (config    . "")
    (compress  . "")
    (folder    . "")
    (image     . "")
    (mindmap   . "")
    (video     . "")
    (audio     . "")
    (secret    . "")
    (font      . "")
    (iso       . ""))
  "")

(defvar mdired--extension-icons nil "")

(defvar mdired-mode-toolbar-map nil
  "The variable which holds the toolbar buttons")

(defvar mdired-preview-buffer-list '()
  )

(setq mdired-preview-buffer-list '())

(defvar-local mdired--dired-buffer nil)
(defvar-local mdired--hl-overlay nil)
(defvar-local mdired--owned nil)

(defvar-local mdired--vector nil
  "The variable which saves the infomation.")

(defvar mdired--preview-mutex nil
  "To prevent many process to do same work.")

(defun mdired (filename)
  "Open mdired operations"
  (interactive "f")
  (let ((dired-hide-details-mode t)
        (dired-free-space nil)
        (directory (if (file-directory-p filename)
                       filename
                     (file-name-parent-directory filename))))
    (unless (file-exists-p mdired-preview-cache-directory)
      (make-directory mdired-preview-cache-directory t))
    (setq-default dired-listing-switches (car (car mdired-listing-switches)))
    (with-current-buffer (dired directory)
      (mdired-build-getter-and-setters)
      (mdired--set-filename mdired--vector filename)
      (mdired-refresh mdired--vector)
      (mdired-toggle-preview))))

(defun find-mdired ()
  (interactive)
  (let ((dired-hide-details-mode t)
        (dired-free-space nil))
    (unless (file-exists-p mdired-preview-cache-directory)
      (make-directory mdired-preview-cache-directory t))
    (setq-default dired-listing-switches (car (car mdired-listing-switches)))
    (find-dired (read-directory-name "Run find in directory: " nil "" t)
                (read-string "Run find (with args): " find-args
                             '(find-args-history . 1)))
    (mdired-first-file)
    (message "begin to refresh ..... %s, %s" (dired-get-filename nil t)
             (buffer-substring (line-beginning-position) (line-end-position)))
    (message "begin to refresh: %s" filename)
    (mdired-build-getter-and-setters)
    (mdired--set-filename mdired--vector filename)
    (mdired-refresh mdired--vector t)
    (mdired-toggle-preview)))

(defun mdired-refresh (vector &optional exclude-main)
  "Refresh this mdired instance."
  (unless exclude-main
    (mdired-find-file (mdired--get-filename vector)))
  (mdired--refresh-path vector)
  (mdired--refresh-parent vector)
  (mdired--refresh-info vector)
  (mdired-ignore-errors
    (mdired--get-preview-window vector)
    (mdired--get-preview-buffer vector)
    (mdired--refresh-preview vector)))

(defun mdired-set-current (filename)
  "Set dired current file to FILENAME if it exists
and is different from current file."
  (if-let (filename
           (absolute-filename (expand-file-name filename))
           (should-set (not (string= absolute-filename
                                     (mdired--get-filename mdired--vector))))
           (file-exists (file-exists-p absolute-filename)))
      (progn (mdired--set-filename mdired--vector absolute-filename) t)
    nil))

;;; Getter and Setters
(defun mdired-build-getter-and-setters ()
  "Build getter and setters"
  (let ((index 0)
        (length (length mdired--vector-conponments)))
    (setq-local mdired--vector (make-vector length nil))
    (dolist (e mdired--vector-conponments)
      (defalias (intern (format "mdired--set-%s" e))
        `(lambda (vector newval)
           (aset vector ,index newval)
           newval))
      (defalias (intern (format "mdired--get-%s" e))
        `(lambda (vector)
           (aref vector ,index)))
      (setq index (1+ index)))))

;;; Helper Functions
(defun mdired-first-file ()
  (goto-char (point-min))
  (while (and (not (dired-get-filename nil t))
              (not (eobp)))
    (dired-next-line 2)))

(defmacro mdired-ignore-errors (window buffer &rest body)
  "A copy of `ignore-errors'"
  (declare (debug t) (indent 0))
  `(condition-case err
       (progn ,@body)
     (error
      (mdired--rewrite-buffer-and-switch
       ,buffer (format "Mdired-Error: %S" err) ,window))))

(defun mdired--rewrite-buffer-and-switch (buffer str &optional window)
  "Try to switch to the BUFFER and show STR"
  (if (buffer-live-p buffer)
      (let ((buffer-read-only nil))
        (with-current-buffer buffer
          (erase-buffer)
          (insert str))
        (when (mdired--check-live-window window)
          (with-selected-window window
            (switch-to-buffer buffer))))
    (message "unable to find [%s][%s], original info: %s" window buffer str)))

(defun mdired--get-dired-filename ()
  "Get filename by using `dired-get-filename'.

If it is on the line which has no file, just return
current directory with a slash in the end."
  (if-let ((filename (dired-get-filename nil t)))
      filename
    (file-name-as-directory default-directory)))

(defun mdired--hl-current-line ()
  (if-let ((overlay mdired--hl-overlay))
      (move-overlay overlay (line-beginning-position) (1+ (line-end-position)))
    (setq-local mdired--hl-overlay
                (make-overlay (line-beginning-position) (1+ (line-end-position))))
    (overlay-put mdired--hl-overlay 'face '((:inherit highlight :extend t)))))

(defun mdired--propertize-hint (string)
  (propertize (concat "[Mdired]: " string) 'face
              '(:inherit highlight :weight bold)))

(defun mdired--remove-slash-more (filename)
  "Remove the current directory from the path.

Maybe nil."
  (if-let ((filename)
           (parent (file-name-parent-directory filename)))
      (if (directory-name-p filename)
          (directory-file-name filename)
        (directory-file-name parent))
    nil))

(defun mdired--file-is-binary (filename)
  "Similiar binary check method from GIT"
  (with-temp-buffer
    (insert-file-contents filename nil 0 8000)
    (goto-char (point-min))
    (and (search-forward "\0" nil 'noerror) t)))

(defun mdired--is-root-dir (filename)
  "Check if we arraived on the root dir."
  (not (file-name-parent-directory filename)))

(defun mdired--check-live-window (window)
  (when (window-live-p window) window))

(defun mdired--try-get-vector ()
  (cond (mdired--vector mdired--vector)
        (mdired--dired-buffer (with-current-buffer mdired--dired-buffer
                                mdired--vector))
        (t nil)))

(defun mdired--try-get-dired-buffer ()
  (cond (mdired--vector (current-buffer))
        (mdired--dired-buffer mdired--dired-buffer)
        (t nil)))

(defun mdired--get-file-type-by-extension (filename)
  (let ((ext (file-name-extension filename)))
    (car-safe (car-safe
               (seq-filter (apply-partially
                            (lambda (e) (member ext (cdr e))))
                           mdired-extensions)))))

(defun mdired--list-files-in-current-buffer (directory-path &optional switches)
  (if switches
      (process-file "ls" nil t nil  "-A" "-p" switches directory-path)
    (process-file "ls" nil t nil  "-A" "-p" directory-path)))

(defun mdired--rebalance-windows (vector)
  (let* ((path-window    (mdired--check-live-window (mdired--get-path-window vector)))
         (dired-window   (mdired--check-live-window (mdired--get-dired-window vector)))
         (parent-window  (mdired--check-live-window (mdired--get-parent-window vector)))
         (preview-window (mdired--check-live-window (mdired--get-preview-window vector)))
         (info-window    (mdired--check-live-window (mdired--get-info-window vector)))
         (total-width (window-width path-window t))
         (f (make-symbol "mdired--resize-by-right"))
         (aoe (make-symbol "mdired--and-or-equal")))
    (fset f (lambda (window total-width percent)
              (when (and window percent)
                (let* ((window-size-fixed nil)
                       (old-width (window-width window t))
                       (width (/ (* total-width percent) 100))
                       (delta (- width old-width)))
                  (adjust-window-trailing-edge window delta t t)))))
    (fset aoe (lambda (e1 e2) (or (and e1 e2) (equal e1 e2))))
    (let ((layouts '((30   70 nil nil)
                     (20   30  50 nil)
                     (20   20  40  20)
                     (20   50 nil  30)
                     (nil 100 nil nil)
                     (nil  40  60 nil)
                     (nil  30  50  20))))
      (dolist (l layouts)
        (when (and (funcall aoe parent-window  (nth 0 l))
                   (funcall aoe dired-window   (nth 1 l))
                   (funcall aoe preview-window (nth 2 l))
                   (funcall aoe info-window    (nth 3 l)))
          (funcall f parent-window  total-width (nth 0 l))
          (funcall f dired-window   total-width (nth 1 l))
          (funcall f preview-window total-width (nth 2 l))
          (funcall f info-window    total-width (nth 3 l)))))))

;;; Dired Window Functions
(defvar-keymap mdired-mode-map
  "<left>"  'mdired-set-current-parent
  "<right>" 'mdired-set-current-child
  "q"       'mdired-quit
  "RET"     (lambda ()
              (interactive)
              (let ((vector mdired--vector))
                (mdired-quit)
                (mdired-quit-all)
                (find-file (mdired--get-filename vector)))))

(define-minor-mode mdired-mode
  "mdired -- just for the main dired window's minor mode"
  :lighter " mdired "
  :global nil
  :group mdired
  )

(defun mdired-find-file (filename)
  "Find the file in the current buffer.

If FILENAME is a directory, just jump into it,
else we will jump into its parent and goto this file."
  (interactive)
  (let ((dired-free-space nil)
        (old-buffer (current-buffer))
        (old-vector mdired--vector)
        (old-listing-switches dired-listing-switches)
        (old-hide-details-mode dired-hide-details-mode))
    ;; If the file is a directory, so we just jump into it,
    ;; else we get into its parent directory and goto this file.
    (setq-default dired-listing-switches old-listing-switches)
    (if (directory-name-p filename)
        (mdired--find-alternate-file mdired--vector filename old-hide-details-mode)
      (let ((directory (file-name-parent-directory filename))
            ;; FIXME: does this work for soft links?
            (file (expand-file-name filename)))
        (unless (string= directory default-directory)
          (mdired--find-alternate-file mdired--vector directory old-hide-details-mode))
        (unless (string= (file-name-nondirectory filename)
                         (dired-get-filename t t))
          (dired-goto-file file))))
    (mdired--hl-current-line)))

(defun mdired--find-alternate-file (vector directory hide-details-mode)
  (let ((kill-buffer-hook nil))
    (setq-local dired-hide-details-mode hide-details-mode)
    (find-alternate-file directory)
    (mdired-mode)
    (setq-local mdired--vector vector)
    (setq-local dired-hide-details-mode hide-details-mode)
    (mdired--dired-add-icon-advices)
    (add-hook 'kill-buffer-hook
               (lambda ()
                (mdired-quit (list (current-buffer))))
              nil t)
    (add-hook 'post-command-hook
              (lambda ()
                (when (bound-and-true-p mdired--vector)
                  (mdired-set-current-by-dired)))
              nil t)
    (mdired--set-toolbar-buttons)
    (mdired--set-dired-buffer mdired--vector (current-buffer))
    (mdired--set-dired-window mdired--vector (selected-window))))

(defun mdired-set-current-by-dired ()
  "Set current file by using `mdired--get-dired-filename'"
  (when (mdired-set-current (mdired--get-dired-filename))
    ;; Highlight current line
    (mdired--hl-current-line)
    (mdired-refresh mdired--vector t)))

(defun mdired-set-current-parent ()
  "Go to the parent dir when we don't on the root dir."
  (interactive)
  ;; Because we can only use this function in the main dired window, so
  ;; mdirediobject-vector can be called quickly.
  (let ((parent (mdired--remove-slash-more
                 (mdired--get-filename mdired--vector))))
    (unless (mdired--is-root-dir parent)
      ;; check if we already arrived on the root dir, so do not
      ;; disturb it.
      (mdired-set-current parent)
      (mdired-refresh mdired--vector))))

(defun mdired-set-current-child ()
  "Goto the selection file/directory, same as `mdired-set-current-parent',
but for child."
  (interactive)
  (let ((filename (mdired--get-filename mdired--vector)))
    (mdired-set-current
     (if (file-directory-p filename)
         (file-name-as-directory filename)
       filename))
    (mdired-refresh mdired--vector)))

(defun mdired--dired-details ()
  "Hide unneed infomation in this dired buffer, such as file infomations
and dired header lines."
  (let ((dired-free-space nil))
    (mdired-hide-header)
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
            (mdired--dired-refresh-icons)
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
      (mdired--set-dired-window
       vector
       (split-window-vertically 2 (mdired--get-dired-window vector)))
      ;; So the window won't change its height
      (setq path-window (mdired--set-path-window vector (selected-window)))
      (select-window (mdired--get-dired-window vector))
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
      (when-let ((vector mdired--vector)
                 (main-window (mdired--get-dired-window vector)))
        (select-window main-window)
        (mdired--set-filename vector path)
        (mdired-refresh vector)))))

(defun mdired-toggle-parent (&rest args)
  (interactive)
  (when-let* ((vector (or (bound-and-true-p mdired--vector)
                          (with-current-buffer mdired--dired-buffer
                            mdired--vector))))
    (if-let ((window (mdired--check-live-window (mdired--get-parent-window vector))))
        (delete-window window)
      (mdired--refresh-parent vector t))
    (mdired--rebalance-windows vector)))

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
      (setq-local mdired--dired-buffer
                  (mdired--get-dired-buffer vector))
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
                       'main-buffer mdired--dired-buffer
                       'help-echo path)
          (setq bs (point)))
        (goto-char (line-end-position))
        ;; (mdired--make-other-path-buttons vector)
        )
      (goto-char (point-max)))))

;;; Parent Window Functions
(defun  mdired--get-or-build-parent-window (vector &optional build)
  (let ((window (mdired--get-parent-window vector))
        parent-window)
    (if (mdired--check-live-window window)
        window
      (when build
        (select-window (mdired--get-dired-window vector))
        (mdired--set-dired-window
         vector
         (split-window-horizontally 25 (mdired--get-dired-window vector)))
        (setq parent-window (mdired--set-parent-window vector (selected-window)))
        (select-window (mdired--get-dired-window vector))
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
                            (mdired--get-dired-window vector)))
             (parent-window (mdired--get-or-build-parent-window vector build))
             (parent-buffer (mdired--get-or-build-parent-buffer vector)))
    (with-selected-window parent-window
      (switch-to-buffer parent-buffer))
    (with-current-buffer parent-buffer
      (setq-local mdired--dired-buffer (mdired--get-dired-buffer vector))
      (let* ((item-path (mdired--remove-slash-more (mdired--get-filename vector)))
             (parent-path (mdired--remove-slash-more item-path))
             (buffer-read-only nil))
        (unless (string= item-path default-directory)
          (unless (string= parent-path (mdired--remove-slash-more default-directory))
            (erase-buffer)
            (when parent-path
              (mdired--list-files-in-current-buffer (mdired--remove-slash-more item-path)))
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
                              :image ,(find-image '((:type svg :file "tm_info.svg")))))
      (define-key map [mdired-toggle-preview]
                  `(menu-item "Preview" mdired-toggle-preview
                              :enable t
                              :help "Toggle Preview Window"
                              :image ,(find-image '((:type svg :file "tm_picture.svg")))))
      (define-key map [mdired-toggle-parent]
                  `(menu-item "Parent" mdired-toggle-parent
                              :enable t
                              :help "Toggle Parent Window"
                              :image ,(find-image '((:type svg :file "mdired-parent.svg")))))
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

;; Thus I don't like to use all-the-icons, so I copied them to here
;; https://github.com/jtbm37/all-the-icons-dired/blob/master/all-the-icons-dired.el
(defun mdired--dired-refresh-advice (fn &rest args)
  "Advice function for FN with ARGS."
  (apply fn args)
  (when mdired-mode
    (mdired--dired-details)
    (mdired--dired-refresh-icons)))

(defun mdired--dired-add-icon-advices ()
  ""
  (when (derived-mode-p 'dired-mode)
    (setq-local tab-width 1)
    (advice-add 'dired-readin :around #'mdired--dired-refresh-advice)
    (advice-add 'dired-revert :around #'mdired--dired-refresh-advice)
    (advice-add 'dired-internal-do-deletions :around #'mdired--dired-refresh-advice)
    (advice-add 'dired-insert-subdir :around #'mdired--dired-refresh-advice)
    (advice-add 'dired-do-kill-lines :around #'mdired--dired-refresh-advice)
    (with-eval-after-load 'dired-narrow
      (advice-add 'dired-narrow--internal :around #'mdired--dired-refresh-advice))
    (mdired--dired-details)
    (mdired--dired-refresh-icons)))

;;; Info Window Functions
(defun mdired--get-or-build-info-window (vector &optional build)
  (let ((window (mdired--get-info-window vector))
        info-window)
    (if (mdired--check-live-window window)
        window
      (when build
        (let ((base-window (or (mdired--check-live-window (mdired--get-preview-window vector))
                               (mdired--check-live-window (mdired--get-dired-window vector)))))
          (select-window base-window)
          (mdired--set-info-window
           vector
           (split-window-horizontally -30 base-window))
          ;; (setq info-window (mdired--set-info-window vector (selected-window)))
          (select-window (mdired--get-dired-window vector))
          (mdired--get-info-window vector))))))

(defun mdired--get-or-build-info-buffer (vector)
  (if-let* ((buffer (mdired--get-info-buffer vector))
            (buffer-live (buffer-live-p buffer)))
      buffer
    (mdired--set-info-buffer
     vector
     (let ((buffer (generate-new-buffer "mdired-info-buffer")))
       (with-current-buffer buffer
         (setq buffer-read-only t)
         (mdired--set-toolbar-buttons)
         (setq mode-line-format "Mdired Info"))
       buffer))))

(defun mdired--line-width-here (&optional pos)
  (car (window-text-pixel-size nil (line-beginning-position) (or pos (point)) t)))

(defun mdired--insert-head-info (icon desc)
  (ignore-errors
    (goto-char (point-min))
    (if icon
        (insert icon " " desc)
      (insert desc))
    (newline)))

(defun mdired--get-file-type (filename)
  (let ((process-file-side-effects))
    (with-temp-buffer
      (process-file "file" nil t t "-b" "--" filename)
      (buffer-substring-no-properties (point-min) (1- (point-max))))))

(defun mdired--insert-preview-common-info (vector)
  (when-let* ((filename (mdired--get-filename vector))
              (window (mdired--get-info-window vector))
              (buffer (mdired--get-info-buffer vector))
              (file-attrs (file-attributes filename))
              (file-type (mdired--get-file-type filename)))
    (with-selected-window window
      (switch-to-buffer buffer))
    (with-current-buffer buffer
      (let ((buffer-read-only nil)
            (window-pixel-width (window-body-width nil t)))
        (mdired--insert-head-info "" (user-login-name (file-attribute-user-id file-attrs)))
        (mdired--insert-head-info "" (group-name (file-attribute-group-id file-attrs)))
        (mdired--insert-head-info "" (file-attribute-modes file-attrs))
        (mdired--insert-head-info "" (file-size-human-readable (file-attribute-size file-attrs)))
        (mdired--insert-head-info "" (format-time-string
                                       "%y-%m-%d %H:%M:%S"
                                       (file-attribute-modification-time file-attrs)))
        (mdired--insert-head-info nil "")
        (mdired--insert-head-info nil file-type)))))

(defun mdired-toggle-info (&rest args)
  (interactive)
  (when-let* ((vector (or (bound-and-true-p mdired--vector)
                          (with-current-buffer mdired--dired-buffer
                            mdired--vector))))
    (if-let ((window (mdired--check-live-window (mdired--get-info-window vector))))
        (delete-window window)
      (mdired--refresh-info vector t))
    (mdired--rebalance-windows vector)))

(defun mdired--refresh-info (vector &optional build)
  (when-let ((dired-window (mdired--check-live-window
                            (mdired--get-dired-window vector)))
             (info-window (mdired--get-or-build-info-window vector build))
             (info-buffer (mdired--get-or-build-info-buffer vector)))
    (with-selected-window info-window
      (switch-to-buffer info-buffer))
    (with-current-buffer info-buffer
      (let ((buffer-read-only nil))
        (setq-local mdired--dired-buffer (mdired--get-dired-buffer vector))
        (erase-buffer)
        ;; TODO: insert specific infomations
        (mdired--insert-preview-common-info vector)))))

;;; Preview Window Functions
(defun mdired--get-or-build-preview-window (vector &optional build)
  (let ((window (mdired--get-preview-window vector))
        preview-window)
    (if (mdired--check-live-window window)
        window
      (when build
        (select-window (mdired--check-live-window (mdired--get-dired-window vector)))
        (mdired--set-preview-window
         vector
         (split-window-horizontally -50 (mdired--check-live-window (mdired--get-dired-window vector))))
        ;; (setq preview-window (mdired--set-preview-window vector (selected-window)))
        (select-window (mdired--get-dired-window vector))
        (mdired--get-preview-window vector)))))

(defun mdired--get-or-build-preview-buffer (vector)
  (if-let* ((buffer (mdired--get-preview-buffer vector))
            (buffer-live (buffer-live-p buffer)))
      buffer
    (mdired--set-preview-buffer
     vector
     (let ((buffer (generate-new-buffer "mdired-preview-buffer")))
       (setq-local mdired--dired-buffer (mdired--get-dired-buffer vector))
       (mdired--set-toolbar-buttons)
       buffer))))

(defun mdired-toggle-preview (&rest args)
  (interactive)
  (when-let* ((vector (or (bound-and-true-p mdired--vector)
                          (with-current-buffer mdired--dired-buffer
                            mdired--vector))))
    (if-let ((window (mdired--check-live-window (mdired--get-preview-window vector))))
        (delete-window window)
      (mdired--refresh-preview vector t))
    (mdired--rebalance-windows vector)))

(defun mdired--refresh-preview (vector &optional build)
  (when-let ((dired-window (mdired--check-live-window
                            (mdired--get-dired-window vector)))
             (preview-window (mdired--get-or-build-preview-window vector build))
             (preview-buffer (mdired--get-or-build-preview-buffer vector)))
    (mdired-ignore-errors
      preview-window
      preview-buffer
      (mdired--preview-file vector preview-buffer))))

(defun mdired--preview-file (vector preserve-buffer)
  "Mdired Preview Core Function.

1. Check if we has permission to preview this file.
   Do this in parent level.
2. Check if this file is binary
   Then use `mdired--preview-binary' to preview this file.
3. Check if this file is text
   Then use `mdired--preview-text' to preview this file.

Return the preview buffer"
  (let* ((filename (mdired--get-filename vector))
         (attrs (file-attributes filename)))
    (cond ((file-directory-p filename)
           (mdired--preview-directory vector preserve-buffer))
          ((= 0 (file-attribute-size attrs))
           (mdired--rewrite-buffer-and-switch preserve-buffer
                                              (mdired--propertize-hint "Empty"))
           (mdired--preview-post-actions vector preserve-buffer))
          ((or (mdired--file-is-binary filename)
               (string= "pdf" (file-name-extension filename)))
           (mdired--preview-binary vector preserve-buffer))
          (t (mdired--preview-text vector filename attrs preserve-buffer)))))

(defun mdired--preview-find-file (vector filename)
  (let ((inhibit-message t)
        (enable-dir-local-variables nil)
        (enable-local-variables :safe)
        (non-essential t)
        (delay-mode-hooks t)
        (vc-follow-symlinks nil)
        (buffer-read-only t)
        (find-file-hook nil))
    (mdired--preview-post-actions
     vector
     (find-file-noselect filename t))))

(defun mdired--preview-post-actions (vector buffer)
  (when-let ((window (mdired--get-preview-window vector)))
    ;; Keep the preview buffer list size in a little value
    (unless (equal buffer (mdired--get-preview-buffer vector))
      (setq mdired-preview-buffer-list
            (delete-dups mdired-preview-buffer-list))
      (delete buffer mdired-preview-buffer-list)
      (while (length> mdired-preview-buffer-list 9)
        (kill-buffer (car (last mdired-preview-buffer-list)))
        (setq mdired-preview-buffer-list (butlast mdired-preview-buffer-list)))
      (push buffer mdired-preview-buffer-list))
    ;; We should switch to the buffer first, so we can set window margins
    (with-selected-window window
      (switch-to-buffer buffer))
    (with-current-buffer buffer
      (setq-local mdired--owned t)
      (setq-local mdired--dired-buffer (mdired--get-dired-buffer vector))
      (mdired--set-toolbar-buttons)
      (set-window-margins window 3 2)
      (setq-local mode-line-format
                  (concat " [MDired] " (file-name-nondirectory (mdired--get-filename vector))))
      (unless (string-prefix-p "mdired-" (buffer-name))
        (rename-buffer (format "mdired-%s" (buffer-name)) t))
      (goto-char (point-min))))
  buffer)


(defun mdired--preview-text (vector filename file-attrs preserve-buffer)
  "This function is just for Text File Preview.

If this file is bigger than `mdired-preview-text-threshold', refuse to view it.
If this file is opened before, use a indirect buffer to view."
  (if (> (file-attribute-size file-attrs) mdired-preview-text-threshold)
      (progn
        (mdired--rewrite-buffer-and-switch
         preserve-buffer
         (with-temp-buffer
           (insert (mdired--propertize-hint "Oversized files, partially previewed"))
           (newline)
           (insert-file-contents filename nil 0 (* (window-height) (window-width)))
           (buffer-string)))
        (mdired--preview-post-actions vector preserve-buffer))
    (if-let* ((buffer (find-buffer-visiting filename)))
        (if-let* ((new-name (concat "mdired-" (buffer-name buffer)))
                  (mdired-buffer (get-buffer new-name)))
            (mdired--preview-post-actions vector mdired-buffer)
          (setq mdired-buffer (make-indirect-buffer buffer new-name t))
          (with-current-buffer mdired-buffer
            (setq-local buffer-read-only t))
          (mdired--preview-post-actions vector mdired-buffer))
      (mdired--preview-find-file vector filename))))

(defun mdired--preview-directory (vector buffer &optional switches)
  (with-current-buffer buffer
    (let ((buffer-read-only nil)
          (filename (mdired--get-filename vector)))
      (setq truncate-lines t)
      (erase-buffer)
      (mdired--list-files-in-current-buffer filename switches)
      (when (= (buffer-size) 0)
        (insert (mdired--propertize-hint "Empty Directory")))))
  (mdired--preview-post-actions vector buffer))

(defun mdired--preview-binary (vector preserver-buffer)
  (let* ((filename (mdired--get-filename vector))
         (ext (downcase (or (file-name-extension filename) "")))
         (type (mdired--get-file-type-by-extension filename))
         (directory (expand-file-name (md5 (file-name-parent-directory filename)) mdired-preview-cache-directory))
         (expanded-filename (expand-file-name (file-name-nondirectory filename) directory))
         preview-filename preview-cmd process)
    (cond ((equal 'image type)
           (setq preview-filename (concat expanded-filename ".png"))
           (setq preview-cmd (mdired--preview-image filename preview-filename)))
          ((equal 'font type)
           (setq preview-filename (concat expanded-filename ".png"))
           (setq preview-cmd (mdired--preview-font filename preview-filename)))
          ((equal 'video type)
           (setq preview-filename (concat expanded-filename ".png"))
           (setq preview-cmd (mdired--preview-video filename preview-filename)))
          ((equal 'compress type)
           (setq preview-filename (concat expanded-filename ".txt"))
           (setq preview-cmd (mdired--preview-compress filename ext preview-filename)))
          ((string= "pdf" ext)
           (setq preview-filename (concat expanded-filename ".png"))
           (setq preview-cmd (mdired--preview-pdf filename preview-filename))))
    (if (and preview-filename preview-cmd
             ;; We only need one process to deal with one file.
             (not (get-process filename)))
        ;; TODO: check whether preview file is older than the original file
        (if (file-exists-p preview-filename)
            (mdired--preview-find-file vector preview-filename)
          (make-directory directory t)
          (setq process (apply #'start-process filename "*mdired-binary-log*" preview-cmd))
          (process-put process 'pf preview-filename)
          (set-process-sentinel
           process
           (lambda (process event)
             (when-let ((vector (mdired--try-get-vector))
                        (filename (process-name process))
                        (samep (string= (mdired--get-filename vector) filename))
                        (preview-filename (process-get process 'pf)))
               (if (not (file-exists-p preview-filename))
                   (message "Unable to create preview for: %s" filename)
                 (mdired--preview-find-file vector preview-filename))))))
      (mdired--rewrite-buffer-and-switch
       preserver-buffer
       (mdired--propertize-hint "Unable to view this file")
       (mdired--get-preview-window vector)))))

(defun mdired--preview-image (filename preview-filename &optional width height)
  (list "convert" filename "-resize" "600" preview-filename))

(defun mdired--preview-pdf (filename preview-filename &optional width height)
  (list "sh" "-c" (format "pdftoppm -f 0 -l 1 -scale-to 600 -W 600 -png \"%s\" > \"%s\"" filename preview-filename)))

(defun mdired--preview-font (filename preview-filename &optional width height)
  (list "convert" "-size" "800x600" "xc:#ffffff" "-font" filename
        "-pointsize" "48" "-gravity" "center" "-fill" "#000000"
        "-annotate" "+0+0" mdired-preview-font-text
        "-flatten" preview-filename))

(defun mdired--preview-video (filename preview-filename)
  (list "ffmpegthumbnailer" "-i" filename "-o" preview-filename "-s600" "-f" "-m" "-q8"))

(defun mdired--preview-compress (filename ext preview-filename)
  (cond ((string= ext "zip")
         (list "sh" "-c" (format "unzip -l \"%s\" > \"%s\"" filename preview-filename)))
        ((string= ext "7z")
         (list "sh" "-c" (format "7z l \"%s\" > \"%s\"" filename preview-filename)))
        ((string= ext "rar")
         (list "sh" "-c" (format "unrar l \"%s\" > \"%s\"" filename preview-filename)))))

;;; Quit related Functions
(defun mdired-quit (&optional exclude-items)
  (interactive)
  (when-let ((vector (mdired--try-get-vector)))
    (mapc
     (lambda (e)
       (unless (member e exclude-items)
         (ignore-errors
           (cond ((bufferp e) (kill-buffer e))
                 ((windowp e) (delete-window e))))))
     vector)
    (mdired-quit-all)))

(defun mdired-quit-all ()
  (interactive)
  (dolist (e (buffer-list))
    (ignore-errors
      (with-current-buffer e
        (when mdired--vector
          (mdired-quit))
        (when mdired--owned
          (kill-buffer))))))


(provide 'mdired)
