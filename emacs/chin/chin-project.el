;;; -*- lexical-binding: t; -*-

(use-package project
  :config
  (add-to-list 'project-vc-ignores ".ccls-cache/")
  (add-to-list 'project-vc-ignores "node_modules")
  (defvar project-language-aware-root-files
    '(
      ".git"
      "tsconfig.json"
      "package.json"
      "Cargo.toml"
      "compile_commands.json"
      "project.clj"
      "compile_flags.txt"))

  (defun project-try-language-aware (dir)
    "Find a super-directory of DIR containing a root file."
    (let ((dir (cl-loop for pattern in project-language-aware-root-files
                        for result = (locate-dominating-file dir pattern)
                        if result return result)))
      (and dir (cons 'language-aware dir))))

  (cl-defmethod project-root ((project (head language-aware)))
    (cdr project))
  (add-hook 'project-find-functions
            #'project-try-language-aware)

  (defvar chin/start-dir default-directory)

  (defun chin/project-save-session (&optional dirpath)
    (interactive)
    (when-let* ((files (delq nil (mapcar #'buffer-file-name (buffer-list))))
                (start-dir (md5 (or dirpath chin/start-dir)))
                (save-dir (expand-file-name "sessions" user-emacs-directory))
                (save-path (expand-file-name start-dir save-dir)))
      (make-directory save-dir t)
      (with-temp-buffer
        (dolist (file files)
            (insert file "\n"))
        (write-file save-path))))

  (defun chin/project-load-session (&optional dirpath)
    (interactive)
    (when-let*
        ((start-dir (md5 (or dirpath chin/start-dir)))
         (save-dir (expand-file-name "sessions" user-emacs-directory))
         (save-path (expand-file-name start-dir save-dir)))
      (when (file-exists-p save-path)
        (dolist (f (split-string
                    (with-temp-buffer
                      (insert-file-contents save-path)
                      (buffer-substring-no-properties
                       (point-min)
                       (point-max)))
                    "\r?\n" t))
          (find-file-noselect f)))))
  (add-hook 'kill-emacs-hook 'chin/project-save-session)
  (add-hook 'emacs-startup-hook 'chin/project-load-session))

(provide 'chin-project)
