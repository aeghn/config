;;; ibuffer-sidebar.el --- Sidebar for `ibuffer' -*- lexical-binding: t -*-

;; Copyright (C) 2018 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; URL: https://github.com/jojojames/ibuffer-sidebar
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1"))
;; Keywords: ibuffer, files, tools
;; HomePage: https://github.com/jojojames/ibuffer-sidebar

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; Provides a sidebar interface similar to `dired-sidebar', but for `ibuffer'.

;;
;; (use-package ibuffer-sidebar
;;   :bind (("C-x C-b" . ibuffer-sidebar-toggle-sidebar))
;;   :ensure nil
;;   :commands (ibuffer-sidebar-toggle-sidebar))
;;

;;; Code:
(require 'ibuffer)
(require 'ibuf-ext)
(require 'face-remap)
(require 'chin-bw-utils)
(eval-when-compile (require 'subr-x))

;; Compatibility

(eval-and-compile
  (with-no-warnings
    (if (version< emacs-version "26")
        (progn
          (defalias 'ibuffer-sidebar-if-let* #'if-let)
          (defalias 'ibuffer-sidebar-when-let* #'when-let)
          (function-put #'ibuffer-sidebar-if-let* 'lisp-indent-function 2)
          (function-put #'ibuffer-sidebar-when-let* 'lisp-indent-function 1))
      (defalias 'ibuffer-sidebar-if-let* #'if-let*)
      (defalias 'ibuffer-sidebar-when-let* #'when-let*))))

;; Customizations
(defgroup ibuffer-sidebar nil
  "A major mode leveraging `ibuffer-sidebar' to display buffers in a sidebar."
  :group 'convenience)

(defcustom ibuffer-sidebar-use-custom-modeline t
  "Show `ibuffer-sidebar' with custom modeline.

This uses format specified by `ibuffer-sidebar-mode-line-format'."
  :type 'boolean
  :group 'ibuffer-sidebar)

(defcustom ibuffer-sidebar-mode-line-format
  '("%e" mode-line-front-space
    mode-line-buffer-identification
    " "  mode-line-end-spaces)
  "Mode line format for `ibuffer-sidebar'."
  :type 'list
  :group 'ibuffer-sidebar)

(defcustom ibuffer-sidebar-display-column-titles nil
  "Whether or not to display the column titles in sidebar."
  :type 'boolean
  :group 'ibuffer-sidebar)

(defcustom ibuffer-sidebar-display-summary nil
  "Whether or not to display summary in sidebar."
  :type 'boolean
  :group 'ibuffer-sidebar)

(defcustom ibuffer-sidebar-width 30
  "Width of the `ibuffer-sidebar' buffer."
  :type 'integer
  :group 'ibuffer-sidebar)

(defcustom ibuffer-sidebar-key-set '("a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k"
                                     "l" "m" "n" "o" "p" "q" "r" "s" "t" "u" "v")
  "Visit window by those keys"
  :type 'list
  :group 'ibuffer-sidebar)

(defcustom ibuffer-sidebar-pop-to-sidebar-on-toggle-open t
  "Whether to jump to sidebar upon toggling open.

This is used in conjunction with `ibuffer-sidebar-toggle-sidebar'."
  :type 'boolean
  :group 'ibuffer-sidebar)

(defcustom ibuffer-sidebar-use-custom-font nil
  "Show `ibuffer-sidebar' with custom font.

This face can be customized using `ibuffer-sidebar-face'."
  :type 'boolean
  :group 'ibuffer-sidebar)

(defcustom ibuffer-sidebar-face nil
  "Face used by `ibuffer-sidebar' for custom font.

This only takes effect if `ibuffer-sidebar-use-custom-font' is true."
  :type 'list
  :group 'ibuffer-sidebar)

(defcustom ibuffer-sidebar-current-buffer-mark ""
  ""
  :type 'string
  :group 'ibuffer-sidebar)

(defcustom ibuffer-sidebar-buffer-mark ""
  ""
  :type 'string
  :group 'ibuffer-sidebar)


(defcustom ibuffer-sidebar-display-alist '((side . left) (slot . 1))
  "Alist used in `display-buffer-in-side-window'.

e.g. (display-buffer-in-side-window buffer '((side . left) (slot . 1)))"
  :type 'alist
  :group 'ibuffer-sidebar)

(defcustom ibuffer-sidebar-buffer-change-commands
  '(ibuffer-sidebar-buffers-add-buffer
    ibuffer-sidebar-refresh-buffer)
  "A list of commands that will executed when buffer changes."
  :type 'list
  :group 'ibuffer-sidebar)

(defcustom ibuffer-sidebar-name "*:Buffers:*"
  "The name of `ibuffer-sidebar' buffer."
  :type 'string
  :group 'ibuffer-sidebar)

(defcustom ibuffer-sidebar-formats
  '((mark " " name))
  "`ibuffer-formats' for `ibuffer-sidebar'."
  :type 'list
  :group 'ibuffer-sidebar)

(defvar ibuffer-sidebar-buffers '()
  "The list holding the buffer list by creating time,
which we used to sort the buffer list.

So we can get a more stable buffer list in the ibuffer-sidebar,
besides we can use `ibuffer-sidebar-previous-buffer' and
`ibuffer-sidebar-next-buffer' to switch between them.")

(defvar ibuffer-sidebar-last-marked-buffer nil
  "The last marked buffer.")

(defvar ibuffer-sidebar-current-overlay
  "The overlay of current line.")


(defvar-keymap ibuffer-sidebar-mode-map
  "RET"            #'ibuffer-sidebar-visit-buffer
  "<mouse-1>"      (lambda (event)
                     (interactive "e")
                     (mouse-set-point event)
                     (ibuffer-sidebar-visit-buffer))
  "M-n"            (lambda ()
                     (interactive)
                     (ibuffer-sidebar-next-buffer ibuffer-sidebar-last-marked-buffer))
  "M-p"            (lambda ()
                     (interactive)
                     (ibuffer-sidebar-previous-buffer ibuffer-sidebar-last-marked-buffer)))

;; Mode

(define-derived-mode ibuffer-sidebar-mode ibuffer-mode
  "Ibuffer-sidebar"
  "A major mode that puts `ibuffer' in a sidebar."
  :group 'ibuffer-sidebar
  (let ((inhibit-read-only t))
    (setq window-size-fixed 'width)

    (when ibuffer-sidebar-use-custom-font
      (ibuffer-sidebar-set-font))

    ;; Remove column titles.
    (unless ibuffer-sidebar-display-column-titles
      (advice-add 'ibuffer-update-title-and-summary
                  :after 'ibuffer-sidebar-format-column-headings))

    ;; Hide summary.
    (unless ibuffer-sidebar-display-summary
      (setq-local ibuffer-display-summary nil))

    (setq-local ibuffer-sorting-mode 'birth)

    ;; Set default format to be minimal.
    (setq-local ibuffer-formats
                (append ibuffer-formats
                        (mapcar
                         (lambda (e)
                           (add-to-list 'e (ibuffer-sidebar-get-prefix
                                            ibuffer-sidebar-buffer-mark)))
                         ibuffer-sidebar-formats)))

    (setq-local ibuffer-current-format (1- (length ibuffer-formats)))
    (ibuffer-update-format)
    (ibuffer-redisplay t)

    (dolist (e (buffer-list))
      (ibuffer-sidebar-buffers-add-buffer e))

    (run-with-idle-timer
     8 1
     #'ibuffer-sidebar-buffers-clear)

    (setq-local ibuffer-name-map nil)

    (when ibuffer-sidebar-use-custom-modeline
      (ibuffer-sidebar-set-mode-line))

    (chin-bw-utils-buffer-change-hook-push 'ibuffer-sidebar-refresh-buffer)))

;; User Interface

;;;###autoload
(defun ibuffer-sidebar-toggle-sidebar ()
  "Toggle the `ibuffer-sidebar' window."
  (interactive)
  (if (ibuffer-sidebar-showing-sidebar-p)
      (ibuffer-sidebar-hide-sidebar)
    (ibuffer-sidebar-show-sidebar)
    (when ibuffer-sidebar-pop-to-sidebar-on-toggle-open
      (pop-to-buffer (ibuffer-sidebar-buffer)))))

;;;###autoload
(defun ibuffer-sidebar-show-sidebar ()
  "Show sidebar with `ibuffer'."
  (interactive)
  (let ((buffer (ibuffer-sidebar-get-or-create-buffer)))
    (display-buffer-in-side-window buffer ibuffer-sidebar-display-alist)
    (let ((window (get-buffer-window buffer)))
      (set-window-dedicated-p window t)
      (set-window-parameter window 'no-delete-other-windows t)
      (set-window-parameter window 'no-other-window t)
      (with-selected-window window
        (let ((window-size-fixed))
          (ibuffer-sidebar-set-width ibuffer-sidebar-width))))
    (ibuffer-sidebar-update-state buffer)))

;;;###autoload
(defun ibuffer-sidebar-hide-sidebar ()
  "Hide `ibuffer-sidebar' in selected frame."
  (ibuffer-sidebar-when-let* ((buffer (ibuffer-sidebar-buffer)))
    (delete-window (get-buffer-window buffer))
    (ibuffer-sidebar-update-state nil)))

(defun ibuffer-sidebar-next-buffer (&optional input-buffer)
  (interactive)
  (ibuffer-sidebar-switch-buffer t input-buffer))

(defun ibuffer-sidebar-previous-buffer (&optional input-buffer)
  (interactive)
  (ibuffer-sidebar-switch-buffer nil input-buffer))

(defun ibuffer-sidebar-do-nothing ()
  (interactive)
  (message "do nothing"))

(defun ibuffer-sidebar-get-prefix (circle &optional _)
  (concat " " circle))

;; Helpers
(defun ibuffer-sidebar-showing-sidebar-p (&optional f)
  "Return whether F or `selected-frame' is showing `ibuffer-sidebar'.

Check if F or `selected-frame' contains a sidebar and return corresponding
buffer if buffer has a window attached to it."
  (ibuffer-sidebar-if-let* ((buffer (ibuffer-sidebar-buffer f)))
      (get-buffer-window buffer)
    nil))

(defun ibuffer-sidebar-get-or-create-buffer ()
  "Get or create a `ibuffer-sidebar' buffer."
  (let ((name ibuffer-sidebar-name))
    (ibuffer-sidebar-if-let* ((existing-buffer (get-buffer name)))
        existing-buffer
      (let ((new-buffer (generate-new-buffer name)))
        (setq ibuffer-sidebar-last-marked-buffer (current-buffer))
        (with-current-buffer new-buffer
          (ibuffer-sidebar-setup))
        new-buffer))))

(defun ibuffer-sidebar-setup ()
  "Bootstrap `ibuffer-sidebar'.

Sets up both `ibuffer' and `ibuffer-sidebar'."
  (ibuffer-mode)
  (ibuffer-update nil)
  (run-hooks 'ibuffer-hook)
  (ibuffer-sidebar-mode)
  (with-eval-after-load "ibuffer"
    (add-to-list 'ibuffer-never-show-predicates
                 (lambda (e)
                   (with-current-buffer e
                     (derived-mode-p 'speedbar-mode))))))

(defun ibuffer-sidebar-get-buffer-this-line ()
  (let* ((sidebar-buffer (ibuffer-sidebar-buffer))
         (sidebar-window (get-buffer-window sidebar-buffer))
         (this-buffer))
    (with-selected-window sidebar-window
      (setq this-buffer
            (car-safe (get-text-property (line-beginning-position) 'ibuffer-properties)))
      (unless this-buffer (message "This line contains no buffer")))
    this-buffer))

(defun ibuffer-sidebar-visit-buffer (&optional buffer)
  "Try to visit buffer from ibuffer-sidebar"
  (interactive)
  (when-let ((buf (or buffer (ibuffer-sidebar-get-buffer-this-line))))
    (if-let ((window-buffer-exists
              (car-safe (seq-filter
                         (apply-partially
                          (lambda (e) (equal (window-buffer e) buf)))
                         (window-list)))))
        (select-window window-buffer-exists)
      (if (chin-bw-utils-select-window)
          (switch-to-buffer buf)
        (ibuffer-visit-buffer buf)))))

(defun ibuffer-sidebar-switch-buffer (forward &optional input-buffer)
  (if-let ((sidebar (ibuffer-sidebar-buffer))
           (current-buffer (if (and input-buffer (buffer-live-p input-buffer))
                               (setq current-buffer input-buffer)
                             (current-buffer))))
      (with-current-buffer sidebar
        (let ((first-cycle-times 0)
              buffer first-buffer last-buffer selected-buffer)
          (goto-char (point-min))
          (while (and (not selected-buffer) (< first-cycle-times 2))
            (ibuffer-forward-line)
            (setq buffer (car-safe (get-text-property (line-beginning-position) 'ibuffer-properties)))
            (when (bufferp buffer)
              (unless first-buffer (setq first-buffer buffer))
              (when (equal buffer first-buffer)
                (setq first-cycle-time (1+ first-cycle-times)))
              (when (and forward (equal last-buffer current-buffer))
                (setq selected-buffer buffer))
              (if (equal buffer current-buffer)
                  (if forward (setq last-buffer buffer)
                    (setq selected-buffer last-buffer))
                (when (not forward) (setq last-buffer buffer)))))
          (if selected-buffer
              (ibuffer-sidebar-visit-buffer selected-buffer)
            (message "No buffer on this direction"))))
    (if forward (next-buffer) (previous-buffer))))

(defun ibuffer-sidebar-buffer (&optional f)
  "Return the current sidebar buffer in F or selected frame.

This returns nil if there isn't a buffer for F."
  (let* ((frame (or f (selected-frame)))
         (buffer (frame-parameter frame 'ibuffer-sidebar)))
    (if (buffer-live-p buffer)
        buffer
      (set-frame-parameter frame 'ibuffer-sidebar nil)
      nil)))

(defun ibuffer-sidebar-focus-or-toggle ()
  "Select ibuffer-sidebar or toggle it"
  (interactive)
  (let ((cur-buf (buffer-name (current-buffer)))
        (win))
    (setq win
          (car-safe
           (seq-filter
            (apply-partially
             (lambda (e)
               (string= (buffer-name (window-buffer e)) ibuffer-sidebar-name)))
            (window-list))))
    (if (and win (not (equal (get-buffer-window) win)))
        (select-window win)
      (ibuffer-sidebar-toggle-sidebar))))

(defun ibuffer-sidebar-update-state (buffer &optional f)
  "Update current state with BUFFER for sidebar in F or selected frame."
  (let ((frame (or f (selected-frame))))
    (set-frame-parameter frame 'ibuffer-sidebar buffer)))

(defun ibuffer-sidebar-buffers-add-buffer (&optional buf)
  "Add buffer"
  (let* ((buffer (if buf buf (current-buffer)))
         (tunple (assq buffer ibuffer-sidebar-buffers))
         index)
    (if tunple
        (cdr-safe tunple)
      (setq index
            (if ibuffer-sidebar-buffers
                (1+ (cl-reduce #'max (mapcar #'cdr-safe ibuffer-sidebar-buffers)))
              0))
      (push (cons buffer index) ibuffer-sidebar-buffers)
      index)))

(defun ibuffer-sidebar-buffers-clear ()
  (setq ibuffer-sidebar-buffers
        (seq-filter
         (apply-partially (lambda (e) (buffer-live-p (car-safe e))))
         ibuffer-sidebar-buffers)))

(defun ibuffer-sidebar-refresh-buffer (&optional current-buffer)
  "Refresh sidebar buffer."
  (ibuffer-sidebar-when-let* ((cur-buf (if current-buffer current-buffer (current-buffer)))
                              (sidebar (ibuffer-sidebar-buffer))
                              (window (get-buffer-window sidebar)))
    (with-current-buffer sidebar
      (setq-local ibuffer-sorting-mode 'birth)
      (ibuffer-update nil t)
      (let ((last-linnum nil)
            (setting t))
        (goto-char (point-min))
        (while (and setting (not (equal last-linnum (line-number-at-pos))))
          (setq last-linnum (line-number-at-pos))
          (forward-line)

          (let* ((r-end (line-end-position))
                 (r-begin (line-beginning-position))
                 (properties (get-text-property r-begin 'ibuffer-properties)))
            (when (equal (car-safe properties) cur-buf)
              (setq ibuffer-sidebar-current-overlay (make-overlay r-begin (1+ r-end)))
              (overlay-put ibuffer-sidebar-current-overlay 'face '((:inherit highlight :extend t)))
              (setq setting nil))))
        (setq-local cursor-in-non-selected-windows nil)
        (beginning-of-line)))))

;; UI

(defun ibuffer-sidebar-format-column-headings (&rest _args)
  "Function ran after `ibuffer-update-title-and-summary' that removes headings.

F should be function `ibuffer-update-title-and-summary'.
ARGS are args for `ibuffer-update-title-and-summary'."
  (interactive)

  (when (and (string= (prin1-to-string major-mode) "ibuffer-sidebar-mode")
             (not ibuffer-sidebar-display-column-titles))
    (with-current-buffer (current-buffer)
      (goto-char 1)
      (re-search-forward "-[ ]*\n" nil t)
      (delete-region 1 (point))
      (let ((window-min-height 1))
        (shrink-window-if-larger-than-buffer))

      (goto-char (point-min))
      (while (re-search-forward "\\[ \\(.*\\) \\]" nil t)
        (let* ((title (match-string 0))
               (property (get-text-property 0 'ibuffer-filter-group-name title)))
          (replace-match "")
          (insert (concat
                   (propertize
                    (format " %s " (substring title 2 -2))
                    'ibuffer-filter-group-name property))))))))

(defun ibuffer-sidebar-set-width (width)
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

(defun ibuffer-sidebar-set-font ()
  "Customize font in `ibuffer-sidebar'.

Set font to a variable width (proportional) in the current buffer."
  (interactive)
  (setq-local buffer-face-mode-face ibuffer-sidebar-face)
  (buffer-face-mode))

(defun ibuffer-sidebar-set-mode-line ()
  "Customize modeline in `ibuffer-sidebar'."
  (setq mode-line-format ibuffer-sidebar-mode-line-format))

;;;###autoload (autoload 'ibuffer-do-sort-by-create-time "create-time")
(define-ibuffer-sorter birth
  "Sort the buffers by their births."
  (:description "birth")
  (let ((birth1 (ibuffer-sidebar-buffers-add-buffer (car a)))
        (birth2 (ibuffer-sidebar-buffers-add-buffer (car b))))
    (>= birth1 birth2)))

(provide 'ibuffer-sidebar)
;;; ibuffer-sidebar.el ends here
