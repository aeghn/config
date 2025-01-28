(defvar-local osi-image-buffer-name "*OSI Image*"
  "Name of the buffer used to display the image.")

(defvar-local osi-current-image nil
  "Stores the current image path displayed in the right window.")

(defvar-local osi-image-window nil
  "The window that shows the image, or nil if not opened.")

(defun osi--get-image-at-point ()
  "Return the image path at the current point."
  (when-let ((link (get-text-property (point) 'htmlize-link)))
    (car-safe (cdr-safe link))))

(defun osi--update-image-window (image-path)
  "Update the right window with the new IMAGE-PATH."
  (let ((old-window (selected-window)))
    (ignore-errors
      (if (not osi-image-window)
          (progn
            (setq osi-image-window (split-window-right))  ; Split the window if no image window
            (select-window osi-image-window)
            (setq osi-current-image image-path)
            (switch-to-buffer (generate-new-buffer osi-image-buffer-name))
            (insert-image (create-image image-path))
            (read-only-mode 1)
            (message "Displaying image: %s" image-path))
        (when (and osi-image-window (not (string= osi-current-image image-path)))

          (select-window osi-image-window)
          (setq buffer-read-only nil)
          (erase-buffer)
          (insert-image (create-image image-path))
          (setq osi-current-image image-path)

          (message "Updated image to: %s" image-path))))
    (select-window old-window)))

(defun osi--check-and-update-image ()
  "Check if the image at the current point has changed and update if necessary."
  (let ((image-path (osi--get-image-at-point)))
    (when image-path
      (osi--update-image-window image-path))))

(defun osi-on-point-change ()
  "Callback function to be run after point moves to check for image update."
  (interactive)
  (message "changed")
  (osi--check-and-update-image))

;;;###autoload
(define-minor-mode osi-mode
  "Minor mode to display images in the right window when the point moves."
  :lighter " OSI"
  :keymap nil
  (if osi-mode
      (add-hook 'post-command-hook 'osi-on-point-change)
    (remove-hook 'post-command-hook 'osi-on-point-change)
    (when osi-image-window
      (delete-window osi-image-window)
      (setq osi-image-window nil))))

(provide 'osi-mode)
