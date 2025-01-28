;;; -*- lexical-binding: t; -*-

(with-eval-after-load 'project
  (add-to-list 'project-vc-ignores ".ccls-cache/")
  (add-to-list 'project-vc-ignores "node_modules")
  (defvar project-language-aware-root-files
    '("tsconfig.json"
      ".git"
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
            #'project-try-language-aware))

(provide 'chin-project)
