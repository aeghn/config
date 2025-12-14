;;; -*- lexical-binding: t; -*-

(setq-default chin/playground-data-dir "~/playground/playground-data"
              chin/docs-dir "~/files/doc")

(when chin/is-windows
  ;; Windows-nt specific settings
  (setq chin/playground-data-dir "F:/playground-data"
        chin/docs-dir "D:/files/doc")
  (let ((msys2root "C:/msys64/"))
    (setenv "PATH" (concat
                    ;; Remember to install `mingw-w64-x86_64-gnupg'
                    "D:/tools/cmd;"
                    msys2root "mingw64/bin" ";"
                    msys2root "ucrt64/bin" ";"
                    msys2root "usr/bin" ";"
                    (getenv "PATH")))
    (setq package-gnupghome-dir (string-replace "c:/" "/c/" (expand-file-name "gnupg" package-user-dir)))
    ;; Without this the new added $PATH value won't be inherite by exec-path
    (setq exec-path (split-string (getenv "PATH") path-separator))))


(use-package dired
  :defer 5
  :config
  (defadvice dired-find-file (around dired-find-file-single-buffer activate) "Replace current buffer if file is a directory." (interactive)
             (let ((orig (current-buffer))
                   (filename (dired-get-file-for-visit))) ad-do-it (when (and (file-directory-p filename)
                                                                              (not (eq (current-buffer) orig)))
                   (kill-buffer orig)))))
(use-package dired-subtree
  :after 'dired
  :ensure t)

(use-package dash
  :ensure t)

(use-package dired-hacks-utils
  :after 'dired
  :after dash
  :config
  (defun chin/once-dired-subtree ()
    (interactive)
    (let ((file (ignore-errors (file-truename (buffer-file-name))))
          (iterp t) start parent dir)
      (project-dired)
      (dired-hide-details-mode 1)
      (when file (setq dir (let ((dir (file-truename default-directory)))
                             (if (string-match ".*/$" dir) dir (concat dir "/"))))
            (setq start (length dir))
            (goto-char (point-min))
            (while-let ((index (or (string-search "/" file start)))
                        (iterp2 t))
              (setq parent (substring file nil index))
              (while (and (not (eobp)) iterp2)
                (dired-next-line 1)
                (when-let* ((fap (dired-file-name-at-point))
                            (samep (file-equal-p parent fap)))
                  (dired-subtree-insert)
                  (setq iterp2 nil)))
              (setq start (1+ index)))
            (while (and iterp (not (eobp)))
              (dired-next-line 1)
              (when-let* ((fap (thing-at-point 'filename))
                          (samep (file-equal-p file (expand-file-name fap parent))))
                (setq iterp nil))))
      (keymap-local-set "<tab>" 'dired-subtree-toggle)
      (keymap-local-set "M-l" 'quit-window)))
  (global-set-key (kbd "M-l") 'chin/once-dired-subtree))

(defun chin/is-private-file (filename)
  (and filename (string-match-p ".*/private/.*" filename)))

(defun chin/add-visited-file ()
  (let ((filename (buffer-file-name)))
    (unless (chin/is-private-file filename)
      (write-region (concat filename "\n") nil chin/hist-files-file t))))

(use-package recentf
  :config
  (recentf-mode 1)
  ;; (defconst chin/hist-files-file "~/.hist-files")
  ;; (defun chin/read-visited-files ()
  ;;   (dolist (f (split-string
  ;;               (with-temp-buffer
  ;;                 (insert-file-contents chin/hist-files-file)
  ;;                 (buffer-substring-no-properties (point-min)
  ;;                                                 (point-max)))
  ;;               "\r?\n" t))
  ;;     (recentf-add-file f)))
  ;; ;; (ignore-errors (chin/read-visited-files))
  ;; (add-hook 'find-file-hook 'chin/add-visited-file)
  )

;; Misc settings
;; Auto save files after losing focus.
;; https://emacs.stackexchange.com/questions/60970/how-to-replace-focus-out-hook-with-after-focus-change-function-in-emacs-27
(add-function :after after-focus-change-function
              (lambda ()
                (save-some-buffers t)))

(defun chin/today-file (directory)
  (interactive)
  (let* ((ct (current-time))
         (sub-dir (format-time-string "%y%m-%d-T%H%M%S" ct))
         (time (completing-read (concat "Timebased file (" directory "): ")
                                (if (file-exists-p directory)
                                    (directory-files directory) nil) nil nil sub-dir))
         (total (expand-file-name time directory)))
    (mkdir (file-name-parent-directory total) t)
    (find-file total)))

(global-set-key (kbd "C-<f5>")
                (lambda ()
                  (interactive)
                  (chin/today-file chin/playground-data-dir)))

(provide 'chin-file)
