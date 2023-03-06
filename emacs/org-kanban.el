;;; org-kanban.el -*- lexical-binding: t; -*-

(defgroup org-kanban nil
  "Kanban for org-mode file"
  :group 'convenience)


(defcustom org-kanban-id-name "KB_ID"
  "Kanban ID for org-mode, if it is set to ID,
So it is org-mode's ID")

(defvar-local org-kanban-file nil
  "Which file is used as a Kanban file")

(defvar-local org-kanban-todo-state nil
  "Which todo state for this buffer to show")

(defvar-local org-kanban-todo-set nil)

(defvar-local org-kanban-window nil)

(defun org-kanban-current-id ()
  (format "kb-%s-%s" (car (time-convert nil t)) (line-number-at-pos)))

(defun org-kanban-real-org-buffer (&optional org-file)
  "Find the buffer visiting the Kanban file, if not, create new one."
  (if-let ((buf (find-buffer-visiting (or org-file org-kanban-file))))
      buf
    ((find-file-noselect org-kanban-file t))))

(defun org-kanban-state-buffer (filename state &optional fill)
  (let* ((name (concat "*" state " | " filename "*"))
         (buf (get-buffer name)))
    (unless buf
      (setq buf (generate-new-buffer name))
      (when fill
        (org-kanban-dispatch-entries (org-kanban-real-org-buffer filename)
                                     (list (cons state buf)))))
    buf))

(defun org-kanban-get-all-state-buffers (filename &optional fill)
  (let* ((org-buffer (org-kanban-real-org-buffer filename))
         (states (org-kanban-todo-set org-buffer))
         sbl)
    (dolist (s states)
      (add-to-list 'sbl (cons s (org-kanban-state-buffer filename s fill)) t))
    sbl))

(defun org-kanban-todo-set (&optional buffer)
  "Get org todo status in current file."
  (with-current-buffer (or buffer (org-kanban-real-org-buffer))
    (car org-todo-sets)))

(defun org-kanban--get-headline-and-description ()
  (buffer-substring-no-properties (org-entry-beginning-position)
                                  (or (save-excursion (outline-next-heading)) (point-max))))

(defun org-kanban-try-put-kanban-id ()
  (if-let ((tid (org-entry-get (point) org-kanban-id-name)))
      tid
    (setq tid (org-kanban-current-id))
    (org-entry-put (point) org-kanban-id-name tid)
    tid))

(defun org-kanban-dispatch-entries (ob sbl &optional eid)
  "We treat the `OB'(original org-mode buffer) as the base buffer,
   traversing all the entries, and dispatching them to the correspoding
   view buffers in the `SBL'(state and buffer list).

   If an `EID'(entry id, must be a level 1 entry) is provided, so just dispatch this entry;"
  (when (seq-empty-p sbl) (signal "State Buffer list is emtpy"))
  (with-current-buffer ob
    ;; Traversing
    (let* ((filename (buffer-file-name ob))
           (todo-set (car org-todo-sets))
           (cache-ht (make-hash-table :test 'equal))
           (rgip (and eid (org-find-property org-kanban-id-name eid)))
           (erasep (not rgip))
           (state-len (length sbl))
           (state-set (mapc #'car sbl))
           begin end ins-pnt parent
           ;; ((id lvl header))
           ancestors)
      (if rgip
          (progn
            (goto-char rgip)
            (setq begin (org-entry-beginning-position)
                  end   (org-end-of-subtree))
            (goto-char begin)
            (when (> (org-current-level) 1)
              (signal "not a level 1 heading")))
        (setq end (point-max)
              begin (point-min))
        (goto-char (point-min))
        (unless (outline-on-heading-p)
          (outline-next-heading)))

      (dolist (sb sbl)
        (with-current-buffer (cdr sb)
          (setq buffer-read-only nil)
          ;; Erase related region
          (if-let ((rgid1 (and eid (org-find-property org-kanban-id-name eid))))
              (progn
                (goto-char rgid1)
                (delete-region (org-entry-beginning-position) (org-end-of-subtree)))
            (when erasep
              (erase-buffer)
              (fundamental-mode)
              (insert (concat "#+TITLE: " (car sb) "\n\n"))))))

      (while (< (point) end)
        (let ((lvl    (org-outline-level))
              (state  (org-get-todo-state))
              (header (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
              (id     (org-kanban-try-put-kanban-id))
              (seg    (org-kanban--get-headline-and-description))
              (arr (make-vector (+ 3 state-len) -1)))
          (aset arr 0 lvl)
          (aset arr 1 header)
          (aset arr 2 id)
          (message "%s" arr)

          ;; find parent
          (setq parent (car-safe ancestors))
          (while (and parent (<= lvl (aref parent 0)))
            (setq ancestors (cdr-safe ancestors))
            (setq parent (car-safe ancestors)))

          (if (null state)
              (if (or (null ancestors) (seq-empty-p ancestors))
                  ;;just drop this
                  (message "Drop it, because it has no parent nor todo state.")
                (setq ins-pnt (point))
                (cl-loop
                 for i from 3
                 for sb in sbl
                 ;; current entry has no state, so it should follow its parent
                 do (when (= (aref parent i) 2)
                      (with-current-buffer (cdr sb)
                        (goto-char ins-pnt)
                        (insert seg)
                        (aset arr i 2)))))

            (cl-loop
             for i from 3
             for sb in sbl
             do (when (string= (car sb) state)
                  (with-current-buffer (cdr sb)
                    (setq ins-pnt (point))
                    (insert seg)
                    (aset arr i 2)
                    (unless (seq-empty-p ancestors)
                      (cl-loop
                       for p in ancestors
                       until (> (aref p i) 0)
                       do
                       (goto-char ins-pnt)
                       (insert (aref p 1) "\n"
                               (make-string (aref p 0) ? ) ":PROPERTIES:\n"
                               (make-string (aref p 0) ? ) ":" org-kanban-id-name ": " (aref p 2) "\n"
                               (make-string (aref p 0) ? ) ":END:\n" )
                       ;; todo: hide this files
                       (aset p i 1))
                      ;; In this case, we should goto the end of the tree, otherwise
                      ;; the trees will be mixed with each other.
                      (goto-char (1+ (org-end-of-subtree))))))))
          (push arr ancestors)

          ;; Go to handle next heading
          (outline-next-heading)))
      (mapcar (lambda (e)
                (with-current-buffer (cdr e)
                  (setq buffer-read-only t)
                  (unless (equal major-mode 'org)
                    (org-mode)
                    (org-kanban-mode))
                  (org-fold-hide-drawer-all)
                  (if eid
                      (org-find-property org-kanban-id-name eid)
                    (goto-char (point-min)))
                  (setq-local org-kanban-file filename)
                  (setq-local org-kanban-todo-state (car e))
                  (setq-local mode-line-format "%b")))
              sbl))))

(defun org-kanban-check-org-mode-buffer (buffer)
  (with-current-buffer buffer
    (goto-char (point-min))
    (unless (outline-on-heading-p)
      (outline-next-heading))
    (if (= (org-current-level) 1)
        t
      (signal 'error "The first entry's level must be 1."))))

(defun org-kanban-build-buffers (file-or-buffer &optional todos)
  (let* ((org-buffer (if (bufferp file-or-buffer)
                         file-or-buffer
                       (find-file-noselect file-or-buffer)))
         (filename (if (bufferp file-or-buffer)
                       (buffer-file-name file-or-buffer)
                     file-or-buffer))
         (fnnd (file-name-nondirectory filename))
         (todo-sets (if todos todos (org-kanban-todo-set org-buffer)))
         (properties-str (with-current-buffer org-buffer
                           (goto-char (point-min))
                           (buffer-substring (point-min) (1- (save-excursion (outline-next-heading))))))
         state-buffer-list)
    (org-kanban-check-org-mode-buffer org-buffer)
    (dolist (state todo-sets)
      (let ((buf (org-kanban-state-buffer filename state)))
        (add-to-list 'state-buffer-list (cons state buf) t)))
    (org-kanban-dispatch-entries org-buffer state-buffer-list)
    state-buffer-list))

(defun org-kanban-fullframe (file-or-buffer)
  (let* ((state-buffer-list (org-kanban-build-buffers file-or-buffer))
         (it state-buffer-list))
    (delete-other-windows)
    (while it
      (let ((buf (cdar it))
            (state (caar it)))
        (switch-to-buffer buf)
        (with-current-buffer buf
          (setq-local org-kanban-window (selected-window)))
        (setq it (cdr-safe it))
        (balance-windows)
        (when it (select-window (split-window-right)))))))

(defun org-kanban--list-next (elem list)
  (car-safe (cdr-safe (member elem list))))

(defun org-kanban--list-previous (elem list)
  (car-safe (cdr-safe (member elem (reverse list)))))

(defun org-kanban-pop-to-state-buffer (buffer)
  (if-let ((win (get-buffer-window buffer)))
      (select-window win)
    (pop-to-buffer-same-window buffer)))

(defun org-kanban-focus-next-buffer ()
  "Try to focus next buffer, if there is a window showing this, switch to it,
else switch to it in current window"
  (interactive)
  (when-let* ((todo-set (org-kanban-todo-set))
              (next-state (org-kanban--list-next org-kanban-todo-state todo-set))
              (buf (org-kanban-state-buffer org-kanban-file next-state t)))
    (org-kanban-pop-to-state-buffer buf)))

(defun org-kanban-focus-previous-buffer ()
  "Try to focus next buffer, if there is a window showing this, switch to it,
else switch to it in current window"
  (interactive)
  (when-let* ((todo-set (org-kanban-todo-set))
              (state (org-kanban--list-previous org-kanban-todo-state todo-set))
              (buf (org-kanban-state-buffer org-kanban-file state t)))
    (org-kanban-pop-to-state-buffer buf)))

(defvar org-kanban-mode-map (make-sparse-keymap)
  "")

(define-minor-mode org-kanban-mode
  "A minor mode for kanban with Org-mode."
  :lighter "KB"
  :require 'org
  :keymap org-kanban-mode-map
  :global nil
  :group org-kanban
  (define-key org-kanban-mode-map "e"   'org-kanban-edit-start)
  (define-key org-kanban-mode-map "RET" 'org-kanban-edit-start)
  (define-key org-kanban-mode-map "b"   'org-kanban-focus-previous-buffer)
  (define-key org-kanban-mode-map "b"   'org-kanban-focus-previous-buffer)
  (define-key org-kanban-mode-map "f"   'org-kanban-focus-next-buffer)
  (define-key org-kanban-mode-map (kbd "M-b") 'org-kanban-focus-previous-buffer)
  (define-key org-kanban-mode-map (kbd "M-f") 'org-kanban-focus-next-buffer))


(defvar-local org-kanban-edit-eid-list nil
  "")

(defvar org-kanban-edit-mode-map (make-sparse-keymap)
  "")

(define-minor-mode org-kanban-edit-mode
  "A minor mode for kanban with Org-mode."
  :lighter "KB Edit"
  :require 'org
  :keymap org-kanban-edit-mode-map
  :global nil
  :group org-kanban)

(defun org-kanban-edit-start (&optional method)
  "METHOD, how did we edit buffer
    - Add sliding
    - Add child
    - Edit"
  (interactive)
  (when-let* ((buffer (or (get-buffer "*ORG-KANBAN-EDIT*")
                          (generate-new-buffer "*ORG-KANBAN-EDIT*")))
              (state-buffer (current-buffer))
              (eid (org-entry-get (point) org-kanban-id-name))
              (org-buffer (org-kanban-real-org-buffer))
              (ori-buffer-position (with-current-buffer org-buffer
                                     (org-find-property org-kanban-id-name eid)))
              (entry (with-current-buffer org-buffer
                       (goto-char ori-buffer-position)
                       (buffer-substring-no-properties
                        (org-entry-beginning-position)
                        (org-end-of-subtree))))
              (lvl-1-eid (with-current-buffer org-buffer
                           (when-let ((pos (org-kanban--level-1-entry-position eid)))
                             (org-entry-get pos org-kanban-id-name))))
              (window (selected-window)))
    (with-selected-window window
      (switch-to-buffer buffer)
      (with-current-buffer buffer
        (erase-buffer)
        (insert "# Buffer for editing Org-Kanban entry")
        (newline)
        (newline)
        (insert entry)
        (org-mode)
        (setq-local org-kanban-file (buffer-file-name org-buffer)
                    org-kanban-state-buffer state-buffer
                    org-kanban-edit-eid eid
                    org-kanban-level-1-eid lvl-1-eid
                    org-kanban-edit-method entry)))
    (org-kanban-set-window-size (max 40 (/ (frame-width) 2)) window)
    (list buffer eid)))

(defun org-kanban-set-window-size (size &optional window)
  (with-selected-window (if window window (selected-window))
    (enlarge-window-horizontally (- size (window-width)))))

(defun org-kanban--level-1-entry-position (eid &optional buffer)
  (when-let* ((eid eid)
              (buf (or buffer (current-buffer)))
              (pos (with-current-buffer buf
                     (org-find-property org-kanban-id-name eid))))
    (with-current-buffer buf
      (goto-char pos)
      (while (and (> (org-current-level) 1)
                  (not (bobp)))
        (outline-previous-heading))
      (if (= (org-current-level) 1)
          (point) nil))))

(defun org-kanban-edit-finish ()
  "Finish the edit buffer, return them to the original buffer and redispatcher them.

1. Get Entry Kanban Id from state buffer.
2. Select from org-mode buffer
2.1 Select the correspording entry from the original Org-mode buffer.
2.2 Select the level 1 entry id from the original Org-mode buffer.
3. Insert the selected entry into the edit buffer(if the edit mothod is edit).
4. EDIT IT.
5. Delete the source part in the original org-mode buffer(if the edit mothod is edit)
6. Get the entities's level less or equal the current  eid"
  (interactive)
  (org-map-entries 'org-kanban-try-put-kanban-id)

  ;; Ensure we are at the first entry.
  (goto-char (point-min))
  (unless (outline-on-heading-p)
    (outline-next-heading))

  (let* ((org-buffer (org-kanban-real-org-buffer))
         (eid org-kanban-edit-eid)
         (sbl (org-kanban-get-all-state-buffers (buffer-file-name org-buffer)))
         (pos (org-find-property org-kanban-id-name eid))
         (seg
          (if pos
              (progn
                (goto-char pos)
                (buffer-substring
                 (org-entry-beginning-position)
                 (org-end-of-subtree)))
            nil)))
    (with-current-buffer org-buffer
      (when-let* ((pos (org-find-property org-kanban-id-name eid)))
        (goto-char pos)
        (delete-region (org-entry-beginning-position)
                       (org-end-of-subtree))
        (when seg (insert seg))))
    (message "dispatch %s %s %s" org-buffer sbl org-kanban-level-1-eid)
    (org-kanban-dispatch-entries org-buffer sbl org-kanban-level-1-eid))
  (balance-windows)
  (kill-buffer (current-buffer))
  (switch-to-buffer org-kanban-state-buffer))

(require 'profiler)
(defun org-kanban-test ()
  (interactive)
  (setq debug-on-error t)
  (with-current-buffer (get-buffer "org-kanban.el")
    (eval-buffer))
  (with-current-buffer (get-buffer "*Messages*")
    (chin/clear-buffer-forcily))
  ;; (profiler-stop)
  ;; (profiler-start 'cpu)
  (org-kanban-fullframe (get-buffer "todo.org"))
  ;; (profiler-stop)
  ;; (profiler-report)
  )
