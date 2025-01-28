;;; -*- lexical-binding: t; -*-

(global-set-key (kbd "C-<right>") 'windmove-right)
(global-set-key (kbd "C-<left>") 'windmove-left)
(global-set-key (kbd "C-<up>") 'windmove-up)
(global-set-key (kbd "C-<down>") 'windmove-down)

(defun chin/select-window ()
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

(global-set-key (kbd "M-j") 'chin/select-window)


;; Message Helper
(defun chin/message-toggle ()
  "Toggle a persistent message popup window.
            If popup is visible but unselected, select it.
            If popup is focused, kill it."
  (interactive)
  (if-let ((win (get-buffer-window "*Messages*")))
      (if (eq (selected-window) win)
          ;; If users attempt to delete the sole ordinary window, silence it.
          (ignore-errors (kill-buffer "*Messages*"))
        (select-window win))
    (switch-to-buffer (get-buffer "*Messages*"))
    (goto-char (point-max))
    ))

(global-set-key (kbd "M-`") 'chin/message-toggle)


(provide 'chin-window-and-buffer)
