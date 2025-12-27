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



(use-package dash
  :ensure t)

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

(global-set-key (kbd "C-<f5>")
                (lambda ()
                  (interactive)
                  (chin/today-file chin/playground-data-dir)))

(provide 'chin-file)
