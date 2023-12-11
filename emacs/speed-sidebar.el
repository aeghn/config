(require 'speedbar)
(require 'project)

;; Customizations
(defgroup speed-sidebar nil
  "A major mode leveraging `speed-sidebar' to display buffers in a sidebar."
  :group 'convenience)

(defcustom speed-sidebar-name "*:SpeedBar:*"
  "The name of `speed-sidebar' buffer."
  :type 'string
  :group 'speed-sidebar)

(defcustom speed-sidebar-head-name "*:SpeedBar-Header:*"
  "The name of `speed-sidebar' buffer."
  :type 'string
  :group 'speed-sidebar)

(defcustom speed-sidebar-display-alist '((side . left) (slot . 2))
  "Alist used in `display-buffer-in-side-window'.

e.g. (display-buffer-in-side-window buffer '((side . left) (slot . 1)))"
  :type 'alist
  :group 'speed-sidebar)

(defcustom speed-sidebar-display-head-alist '((side . left) (slot . -1))
  "Alist used in `display-buffer-in-side-window'.

e.g. (display-buffer-in-side-window buffer '((side . left) (slot . 1)))"
  :type 'alist
  :group 'speed-sidebar)

(defcustom speed-sidebar-width 30
  "Width of the `speed-sidebar' buffer."
  :type 'integer
  :group 'speed-sidebar)

(defcustom speed-sidebar-face nil
  "Face used by `speed-sidebar' for custom font.

This only takes effect if `speed-sidebar-use-custom-font' is true."
  :type 'list
  :group 'speed-sidebar)

(defcustom speed-sidebar-mode-line-format
  '("*:SpeedBar:*")
  "Mode line format for `speed-sidebar'."
  :type 'list
  :group 'speed-sidebar)

(defface speed-sidebar-selected-face
  '((t (:background "#eeccaa"
                    :foreground "#000000"
                    :extend t)))
  "Face for speed-sidebar selected file line"
  :group 'speed-sidebar)

(defvar speed-sidebar-selected-line-overlay nil
  "Overlay for speed-sidebar selected file line")

(defcustom speed-sidebar-expand-icon-button-alist
  '(("<+>" . "")
    ("<->" . "")
    ("< >" . "")
    ("[+]" . "")
    ("[-]" . "")
    ("[?]" . "")
    ("[ ]" . "")
    ("{+}" . "")
    ("{-}" . "")
    ("<M>" . "")
    ("<d>" . "")
    ("<i>" . "")
    (" =>" . "")
    (" +>" . "")
    (" ->" . "")
    (">"   . "")
    ("@"   . "")
    ("  @" . "")
    ("*"   . "")
    ("%"   . "")
    ("#"   . "")
    ("//"  . "")
    ("!"   . ""))
  "Font icons for speed-sidebar"
  :type 'list
  :group 'speed-sidebar)

(define-derived-mode speed-sidebar-mode speedbar-mode
  "speed-sidebar"
  "A major mode that puts `speedbar' in a sidebar."
  :group 'speed-sidebar
  (let ((inhibit-read-only t))
    (setq window-size-fixed 'width)
    (speed-sidebar--set-icons)
    (speed-sidebar--set-font)
    (speed-sidebar--set-mode-line)
    (speed-sidebar--override-keys)
    (base-sidebar-buffer-change-hook-push 'speed-sidebar-refresh-buffer)))

;;;###autoload
(defun speed-sidebar-toggle-sidebar ()
  "Toggle the `speed-sidebar' window."
  (interactive)
  (if (speed-sidebar--showing-sidebar-p)
      (speed-sidebar-hide-sidebar)
    (speed-sidebar-show-sidebar)))

;;;###autoload
(defun speed-sidebar-show-sidebar ()
  "Show sidebar with `speedbar'."
  (interactive)
  (let ((buffer (speed-sidebar--get-or-create-buffer)))
    (display-buffer-in-side-window buffer speed-sidebar-display-alist)
    (when-let ((window (get-buffer-window buffer)))
      (with-selected-window window
        (let ((window-size-fixed))
          (set-window-dedicated-p window t)
          (set-window-parameter window 'no-delete-other-windows t)
          (set-window-parameter window 'no-other-window t)
          (speed-sidebar--set-width speed-sidebar-width)))
      (speed-sidebar-refresh-buffer t))))

;;;###autoload
(defun speed-sidebar-hide-sidebar ()
  "Hide `speed-sidebar' in selected frame."
  (when-let* ((buffer (speed-sidebar--buffer)))
    (delete-window (get-buffer-window buffer))))

;;;###autoload
(defun speed-sidebar-focus-or-toggle ()
  "Select speed-sidebar or toggle it"
  (interactive)
  (let ((cur-buf (buffer-name (current-buffer)))
        (win))
    (setq win
          (car-safe (seq-filter (apply-partially
                                 (lambda (e)
                                   (string= (buffer-name (window-buffer e)) speed-sidebar-name)))
                                (window-list))))
    (if (and win (not (equal (get-buffer-window) win)))
        (select-window win)
      (speed-sidebar-toggle-sidebar))))

(defun speed-sidebar--showing-sidebar-p (&optional f)
  "Return whether F or `selected-frame' is showing `speed-sidebar'.

Check if F or `selected-frame' contains a sidebar and return corresponding
buffer if buffer has a window attached to it."
  (if-let* ((buffer (speed-sidebar--buffer f)))
      (get-buffer-window buffer)
    nil))

(defun speed-sidebar-visit-file ()
  "Try to visit file from speed-sidebar"
  (interactive)
  (when-let* ((file (speedbar-line-file))
              (buf (find-file-noselect file)))
    (when buf
      (setq window-buffer-exists
            (car-safe (seq-filter (apply-partially (lambda (e) (equal (window-buffer e) buf)))
                                  (window-list))))
      (if window-buffer-exists
          (select-window window-buffer-exists)
        (if (base-sidebar-select-window)
            (switch-to-buffer buf)
          (ibuffer-visit-buffer buf))))))

(defun speed-sidebar--override-keys ()
  (let ((ori-fun (keymap-lookup speedbar-file-key-map "RET")))
    (define-key speedbar-file-key-map (kbd "M-RET") ori-fun))
  (define-key speedbar-file-key-map (kbd "<mouse-1>") #'speed-sidebar-visit-file)
  (define-key speedbar-file-key-map (kbd "<mouse-2>") #'speed-sidebar-visit-file)
  (define-key speedbar-file-key-map (kbd "RET") #'speed-sidebar-visit-file)
  (define-key speedbar-file-key-map (kbd "<return>") #'speed-sidebar-visit-file))

(defun speed-sidebar--expand-to-current-file (filename)
  "Make speedbar expand to the line of current file."
  (when-let ((buffer-file (expand-file-name filename))
             (process t))
    (with-current-buffer (speed-sidebar--buffer)
      (goto-char (point-min))
      (while (and (< (point) (point-max)) process)
        (let ((f (speedbar-line-file)))
          (when (and f (file-directory-p f) (string-prefix-p (concat f "/") buffer-file))
            (save-excursion (speedbar-expand-line)))
          (if (string= buffer-file f)
              (setq process nil)
            (forward-line)))))))

(defun speed-sidebar-refresh-buffer (&optional force)
  "Refresh the sidebar buffer"
  (interactive)
  (when-let* ((buffer (current-buffer))
              (speed-buffer (speed-sidebar--buffer))
              (window (get-buffer-window speed-buffer))
              (file-or-force (or (buffer-file-name) force)))

    (setq default-directory (speed-sidebar--find-root))
    (speedbar-refresh)
    (unless (booleanp file-or-force)
      (with-selected-window window
        (setq-local cursor-in-non-selected-windows nil)
        (speed-sidebar--expand-to-current-file file-or-force)
        (when speed-sidebar-selected-line-overlay
          (delete-overlay speed-sidebar-selected-line-overlay))
        (setq speed-sidebar-selected-line-overlay
              (make-overlay (line-beginning-position) (1+ (line-end-position))))
        (overlay-put speed-sidebar-selected-line-overlay
                     'face 'speed-sidebar-selected-face)
        (speedbar-recenter)
        (let ((current-directory (file-name-directory file-or-force)))
          (when current-directory
            (with-current-buffer buffer
              (setq default-directory current-directory))))))))


(defun speed-sidebar--find-root ()
  "Find the project root of current file, which should as
the default directory of speed-sidebar buffer"
  (if-let* ((pc (project-current))
            (dir (if (fboundp 'project-root)
                     (project-root pc)
                   (car (with-no-warnings (project-roots pc))))))
      dir
    (expand-file-name default-directory)))

(defun speed-sidebar--buffer (&optional f)
  "Get speed-sidebar buffer"
  (get-buffer speed-sidebar-name))

(defun speed-sidebar--get-or-create-buffer ()
  "Get or create a `speed-sidebar' buffer."
  (if-let* ((name speed-sidebar-name)
            (existing-buffer (get-buffer name)))
      existing-buffer
    (let ((new-buffer (generate-new-buffer name))
          (current-window (selected-window))
          (truncate-lines t))
      (setq speedbar-buffer new-buffer
            speedbar-frame (selected-frame)
            dframe-attached-frame (selected-frame)
            speedbar-select-frame-method 'attached
            speedbar-verbosity-level 0
            speedbar-use-images nil
            speedbar-last-selected-file nil
            speedbar-update-flag nil
            speedbar-update-flag-disable t
            truncate-lines t)
      (set-buffer speedbar-buffer)
      (speedbar-mode)
      (speedbar-update-contents)
      (speed-sidebar-mode)
      new-buffer)))

(defun speed-sidebar--set-width (width)
  "Set the width of the buffer to WIDTH when it is created."
  ;; Copied from `treemacs--set-width' as well as `neotree'.
  (unless (one-window-p)
    (let ((window-size-fixed)
          (w (max width window-min-width)))
      (cond
       ((> (window-width) w)
        (shrink-window-horizontally  (- (window-width) w)))
       ((< (window-width) w)
        (enlarge-window-horizontally (- w (window-width))))))))

(defun speed-sidebar--set-font ()
  "Customize font in `speed-sidebar'.

Set font to a variable width (proportional) in the current buffer."
  (when-let ((buffer (speed-sidebar--buffer))
             (face speed-sidebar-face))
    (with-current-buffer buffer
      (setq-local buffer-face-mode-face face)
      (buffer-face-mode))))

(defun speed-sidebar--set-mode-line ()
  "Customize modeline in `speed-sidebar'."
  (when-let ((buffer (speed-sidebar--buffer))
             (format speed-sidebar-mode-line-format))
    (with-current-buffer buffer
      (setq-local mode-line-format format))))

(defun speed-sidebar--set-icons ()
  "Use `speed-sidebar-expand-icon-button-alist' fonticons to format buffer"
  (advice-add
   'speedbar-insert-image-button-maybe
   :before
   (lambda (start length)
     (let* ((end (+ start length))
            (str (buffer-substring-no-properties start end))
            (marker))
       (setq marker (car-safe
                     (seq-filter
                      (apply-partially (lambda (e) (string= str (car e))))
                      speed-sidebar-expand-icon-button-alist)))
       (when marker
         (put-text-property start end 'face '(:inherit default :underline nil))
         (put-text-property start end 'display (cdr marker))))
     (setq speedbar-use-images nil))))
