;;; -*- lexical-binding: t; -*-

(use-package comment-dwim-2
  :ensure t
  :config
  (global-set-key (kbd "M-;") 'comment-dwim-2))

(use-package yafolding
  :ensure t
	     :config
  (add-hook 'prog-mode-hook 'yafolding-mode))

(use-package expand-region
  :ensure t
  :config
  (define-advice set-mark-command (:before-while (arg))
    "Repeat C-SPC to expand region."
    (interactive "P")
    (if (eq last-command 'set-mark-command)
        (progn
          (er/expand-region 1)
          nil)
      t)))

(use-package point-stack
	     :config
  (point-stack-setup-advices)
  (global-set-key (kbd "M-1") 'point-stack-pop)
  (global-set-key (kbd "M-2") 'point-stack-forward-stack-pop))

(defun chin/indent-current-buffer ()
  "indent current buffer"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))

(global-set-key (kbd "C-c i") 'chin/indent-current-buffer)

(defun chin/delete-blanks (&optional insert-blank-p)
  (interactive)
  (let* ((end-pos (progn (back-to-indentation)
                         (point)))
         (start-pos (1+ (search-backward-regexp "[^\n[:space:]]"))))
    (delete-region start-pos end-pos)
    (forward-char)
    (unless insert-blank-p
      (insert " "))))

(global-set-key (kbd "M-h") 'chin/delete-blanks)
(global-set-key (kbd "C-M-h") (lambda () (interactive) (chin/delete-blanks t)))

(defun chin/insert-tab-char ()
  "Insert a tab char. (ASCII 9, \t)"
  (interactive)
  (insert "\t"))

(global-set-key (kbd "C-t") 'chin/insert-tab-char)

(defun chin/move-beginning-of-line ()
  "Move point back to indentation of beginning of line or beginning of line."
  (interactive)
  (let ((orig-begin (point)))
    (back-to-indentation)
    (if (= orig-begin (point))
        (beginning-of-line))))

(global-set-key (kbd "C-a") 'chin/move-beginning-of-line)
(defun chin/revert-buffer ()
  "Revert buffer without confirming."
  (interactive)
  (revert-buffer t t t)
  (message "buffer is reverted"))

(global-set-key (kbd "M-r") 'chin/revert-buffer)

(defun chin/clear-buffer-forcily ()
  (interactive)
  (let ((buffer-read-only nil))
    (erase-buffer)))

(defun chin/match-paren (arg)
  "Go to the matching paren if on a paren; otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))

(global-set-key (kbd "C-c '") 'chin/match-paren)

(setq kill-whole-line t)


(provide 'chin-edit)
