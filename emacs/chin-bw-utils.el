;; Customizations
(defgroup chin-bw-utils nil
  "A major mode leveraging `ibuffer-sidebar' to display buffers in a sidebar."
  :group 'convenience)

(defcustom chin-bw-utils-key-set '("a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k"
                                   "l" "m" "n" "o" "p" "q" "r" "s" "t" "u" "v")
  "Visit window by those keys"
  :type 'list
  :group 'chin-bw-utils)

(defvar chin-bw-utils-buffer-change-functions '()
  "")

(defvar chin-bw-utils-buffer-last-file-buffer ""
  "")

(defun chin-bw-utils-buffer-change-hook-push (funcname)
  (when (fboundp funcname)
    (unless (fboundp 'chin-bw-utils-buffer-change-do-functions)
      (defalias (intern "chin-bw-utils-buffer-change-do-functions")
        (function (lambda (&rest _)
                    (when chin-bw-utils-buffer-change-functions
                      (dolist (f chin-bw-utils-buffer-change-functions)
                        (funcall f)))))))
    (unless (fboundp 'chin-bw-utils-window-buffer-func)
      (defalias (intern "chin-bw-utils-window-buffer-func")
        (function (lambda (&rest _)
                    (add-hook 'window-buffer-change-functions
                              #'chin-bw-utils-buffer-change-do-functions)))))

    (unless (fboundp 'chin-bw-utils-window-selection-func)
      (defalias (intern "chin-bw-utils-window-selection-func")
        (function (lambda (&rest _)
                    (chin-bw-utils-window-buffer-func)
                    (chin-bw-utils-buffer-change-do-functions)))))

    (unless (member funcname chin-bw-utils-buffer-change-functions)
      (push funcname chin-bw-utils-buffer-change-functions))

    (add-hook 'window-selection-change-functions
              #'chin-bw-utils-window-selection-func)))

(defun chin-bw-utils-select-window ()
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
                (setq key (nth loop chin-bw-utils-key-set))
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

(defun chin-bw-utils--set-buffer ()
  (when (buffer-file-name)
    (setq chin-bw-utils-buffer-last-file-buffer (current-buffer))))

(defun chin-bw-utils-check-side (&optional window)
  (let ((win (if window window (selected-window))))
    (if (window-parameter win 'window-side)
        t nil)))

(chin-bw-utils-buffer-change-hook-push
 'chin-bw-utils--set-buffer)

(provide 'chin-bw-utils)
