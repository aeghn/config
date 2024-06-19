;; This file should be put at: C:\Program Files\Emacs\emacs-29.3_2\share\emacs\site-lisp\site-start.el

;; https://emacs.stackexchange.com/a/48029
;; Shortcut settings: "C:\Program Files\Emacs\emacs-29.3_2\bin\emacsclientw.exe" -n -c -a "" --server-file "D:\chin-home\.emacs.d\server\server"

(setq user-init-file       "D:/files/config/emacs/init.el"
      user-emacs-directory "D:/chin-home/.emacs.d"
      default-directory    "F:/playground-data")

(setenv "HOME" "D:/chin-home")

(load user-init-file)