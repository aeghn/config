;;; -*- lexical-binding: t; -*-

(defun chin/insert-half-width-space-between-chinese-and-english ()
  "在中英文之间增加半角空格"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "\\([a-zA-Z0-9]\\)\\([^\x00-\xff]\\)" nil t)
      (replace-match "\\1 \\2"))
    (goto-char (point-min))
    (while (re-search-forward "\\([^\x00-\xff]\\)\\([a-zA-Z0-9]\\)" nil t)
      (replace-match "\\1 \\2"))))

(provide 'chin-chinese)
