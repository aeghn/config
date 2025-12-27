;;; aml-mode.el --- An Emacs major mode for AML

;; Copyright (C) 2012  David Christiansen

;; Author: David Christiansen <drc@itu.dk>
;; Keywords: languages

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This is a major mode for working with AML code.

;; Installation instructions: Place the file aml-mode.el somewhere on your
;; load-path.  Then, add (require 'aml-mode) to your .emacs.  Finally, it may
;; be a good idea to add aml-mode to your auto-mode-alist. For example, you
;; can write (add-to-list 'auto-mode-alist '("\\.aml$" . aml-mode)) .

;;; Code:


(defvar aml-keywords
  '("value"
    "variable"
    "statemodel"
    "riskmodel"
    "contract"
    "function"
    "type"
    "where"
    "pays"))

(defvar aml-operators
  '(":" "=>" "->" "+" "-" "*" "/" "<" ">" ">=" "<=" "="))

(defvar aml-font-lock-defaults
  `((
     ("\"\\.\\*\\?" . font-lock-string-face)
     (,(regexp-opt aml-operators 'symbols) . font-lock-builtin-face)
     (,(regexp-opt aml-keywords 'words) . font-lock-keyword-face)
     )))

(defgroup aml nil "AML mode customization" :prefix 'aml)

(defcustom aml-tab-width 2
  "Width to indent AML"
  :type 'integer
  :group 'aml)

;;; Derived from haskell-simple-indent
(defun aml-indent ()
  "Space out to under next visible indent point.
Indent points are positions of non-whitespace following
whitespace in lines preceeding point and the current tab width
past the first such position.  A position is visible if it is to
the left of the first non-whitespace of every nonblank line
between the position and the current line.  If there is no
visible indent point beyond the current column, indentation
resets to the previous line."
  (interactive)
  (when (or (not (integerp aml-tab-width))
            (< aml-tab-width 1))
    (error "aml-tab-width must be at least 1. Current value: %s" aml-tab-width))
  (let* ((start-column (current-column))
         (invisible-from nil)		; `nil' means infinity here
         (indent
          (catch 'aml-simple-indent-break
            (save-excursion
              (while (progn (beginning-of-line)
                            (not (bobp)))
                (forward-line -1)
                (if (not (looking-at "[ \t]*\n"))
                    (let ((this-indentation (current-indentation)))
                      ; First indent to previous non-empty line's start column
                      (when (< start-column this-indentation)
                        (throw 'aml-simple-indent-break this-indentation))
                      ; Then indent to the tab-width past that if the previous line
                      ; ends with '=' or 'where'
                      (when (and (< start-column
                                    (+ this-indentation aml-tab-width))
                                 (save-excursion
                                   (beginning-of-line)
                                   (looking-at ".*[ \t]*\\(=\\|[ \t]where\\)[ \t]*$")))
                        (throw 'aml-simple-indent-break (+ this-indentation aml-tab-width)))
                      ; Now take indentation points one at a time
                      (when (or (not invisible-from)
                                (< this-indentation invisible-from))
                        (if (> this-indentation start-column)
                          (setq invisible-from this-indentation)
                          (let ((end (line-beginning-position 2)))
                            (move-to-column start-column)
                            ;; Is start-column inside a tab on this line?
                            (if (> (current-column) start-column)
                                (backward-char 1))
                            (or (looking-at "[ \t]")
                                (skip-chars-forward "^ \t" end))
                            (skip-chars-forward " \t" end)
                            (let ((col (current-column)))
                              (throw 'aml-simple-indent-break
                                     (if (or (= (point) end)
                                             (and invisible-from
                                                  (> col invisible-from)))
                                       invisible-from
                                       col)))))))))))))
    (if indent
	(let ((opoint (point-marker)))
	  (indent-line-to indent)
	  (if (> opoint (point))
	      (goto-char opoint))
	  (set-marker opoint nil))
      (indent-line-to 0))))


(define-derived-mode aml-mode fundamental-mode "AML"
  "AML Mode is a mode for writing AML products and computations."
  :group 'aml

  (setq font-lock-defaults aml-font-lock-defaults)

  (setq comment-column 0)
  (setq comment-start "(*")
  (setq comment-end " *)")

  ;; Match comments with each other
  (modify-syntax-entry ?\( "() 1bn" aml-mode-syntax-table)
  (modify-syntax-entry ?\* ". 23bn" aml-mode-syntax-table)
  (modify-syntax-entry ?\) ")( 4bn" aml-mode-syntax-table)
  (modify-syntax-entry ?\/ ". 12" aml-mode-syntax-table)
  (modify-syntax-entry ?\n ">" aml-mode-syntax-table)

  ;; Match other kinds of parens
  (modify-syntax-entry ?\[ "(]" aml-mode-syntax-table)
  (modify-syntax-entry ?\] ")[" aml-mode-syntax-table)
  (modify-syntax-entry ?\{ "(}" aml-mode-syntax-table)
  (modify-syntax-entry ?\} "){" aml-mode-syntax-table)

  ;; Strings
  (modify-syntax-entry ?\" "\"" aml-mode-syntax-table)
  (modify-syntax-entry ?\\ "\\" aml-mode-syntax-table)

  (set (make-local-variable 'indent-line-function) 'aml-indent)
)

(provide 'aml-mode)
;;; aml-mode.el ends here
