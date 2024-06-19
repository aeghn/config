;; TODO remove this
(setq package-check-signature nil)
(setq package-user-dir "~/.emacs.d/packages")

;; Windows-nt specific settings
(setq chin/playground-data-dir "F:/playground-data"
      chin/docs-dir "D:/files/docs")
(let ((msys2root "C:\\msys64\\"))
  (setenv "PATH" (concat
                  ;; Remember to install `mingw-w64-x86_64-gnupg'
                  "D:\\tools\\cmd;"
                  msys2root "mingw64\\bin" ";"
                  msys2root "mingw64\\x86_64-w64-mingw32\\bin" ";"
                  msys2root "usr\\bin" ";"
                  (getenv "PATH")))
  (setq package-gnupghome-dir (string-replace "c:/" "/c/" (expand-file-name "gnupg" package-user-dir)))
  ;; Without this the new added $PATH value won't be inherite by exec-path
  (setq exec-path (split-string (getenv "PATH") path-separator)))
