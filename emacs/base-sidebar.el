;; Customizations
(defgroup base-sidebar nil
  "A major mode leveraging `ibuffer-sidebar' to display buffers in a sidebar."
  :group 'convenience)

(defcustom base-sidebar-key-set '("a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k"
                                   "l" "m" "n" "o" "p" "q" "r" "s" "t" "u" "v")
  "Visit window by those keys"
  :type 'list
  :group 'base-sidebar)

(defvar base-sidebar-buffer-change-functions '()
  "")

(defvar base-sidebar-buffer-last-file-buffer ""
  "")

(defun base-sidebar-buffer-change-hook-push (funcname)
  (when (fboundp funcname)
    (unless (fboundp 'base-sidebar-buffer-change-do-functions)
      (defalias (intern "base-sidebar-buffer-change-do-functions")
        (function (lambda (&rest _)
                    (when base-sidebar-buffer-change-functions
                      (dolist (f base-sidebar-buffer-change-functions)
                        (funcall f)))))))
    (unless (fboundp 'base-sidebar-window-buffer-func)
      (defalias (intern "base-sidebar-window-buffer-func")
        (function (lambda (&rest _)
                    (add-hook 'window-buffer-change-functions
                              #'base-sidebar-buffer-change-do-functions)))))

    (unless (fboundp 'base-sidebar-window-selection-func)
      (defalias (intern "base-sidebar-window-selection-func")
        (function (lambda (&rest _)
                    (base-sidebar-window-buffer-func)
                    (base-sidebar-buffer-change-do-functions)))))

    (unless (member funcname base-sidebar-buffer-change-functions)
      (push funcname base-sidebar-buffer-change-functions))

    (add-hook 'window-selection-change-functions
              #'base-sidebar-window-selection-func)))

(defun base-sidebar-select-window ()
  (interactive)
  (let ((normal-window-list)
        (window-list-size)
        (selected-window))
    (setq normal-window-list
          (seq-filter
           (apply-partially (lambda (e) (not (window-dedicated-p e))))
           (window-list)))
    (setq window-list-size (length normal-window-list))
    (setq selected-window
          (if (= window-list-size 1)
              (car-safe normal-window-list)
            (let ((ovs nil)
                  (loop 0)
                  (ov nil)
                  (key)
                  (key-wins nil))
              (dolist (win normal-window-list)
                (setq key (nth loop base-sidebar-key-set))
                (push (list key win) key-wins)
                (select-window win)
                (let ((ov (make-overlay (window-start) (1+ (window-start)))))
                  (push ov ovs)
                  (overlay-put ov 'face '((:height 3.0 :foreground "#aa0000")))
                  (overlay-put ov 'display (format "[%s]" key))
                  (overlay-put ov 'window win))
                (setq loop (1+ loop)))
              (setq win-key (ignore-errors (char-to-string (read-char "Please enter key"))))
              (unless win-key (setq win-key ""))
              (setq selected-win
                    (car-safe
                     (seq-filter (apply-partially (lambda (e) (string= (car-safe e) win-key)))
                                 key-wins)))
              (mapcar #'delete-overlay ovs)
              (car-safe (cdr-safe selected-win)))))
    (when selected-window (select-window selected-window))
    selected-window))

(defun base-sidebar--set-buffer ()
  (when (buffer-file-name)
    (setq base-sidebar-buffer-last-file-buffer (current-buffer))))

(defun base-sidebar-check-side (&optional window)
  (let ((win (if window window (selected-window))))
    (if (window-parameter win 'window-side)
        t nil)))

(base-sidebar-buffer-change-hook-push
 'base-sidebar--set-buffer)

(provide 'base-sidebar)
