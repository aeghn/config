(setq-default work/base-dir "e:/work")
(setq-default work/todo-file (expand-file-name "todo.org" work/base-dir))

(push work/todo-file org-agenda-files)

(global-set-key (kbd "C-<f5>")
                (lambda ()
                  (interactive)
                  (chin/today-file work/base-dir)))

(global-set-key (kbd "<f5>")
                (lambda ()
                  (interactive)
                  (chin/todo work/todo-file)))

                
