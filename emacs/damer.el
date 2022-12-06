;;; damer.el --- A Database First-class Data Manager Based on Sqlite3 -*- lexical-binding: t; -*-

(defgroup damer nil
  "DAta ManagER"
  :group 'convenience)

(defvar-local damer-database-path nil
  "The datamanager database path.")
(defvar-local damer-database nil
  "The datamanager database.")

(defconst damer-categories
  '(("item"  . "")
    ("folder" . "")
    ("file"   . "")
    ("web"    . "")
    ("pdf"    . "")
    ("audio"  . "")
    ("video"  . "")
    ("game"   . ""))
  "The categories and their icons")

(defvar-keymap damer-mode-map
  :doc "Keymap for damer-mode"
  "q"          #'damer-quit
  "M-RET"      #'damer-insert-child
  "RET"        #'damer-insert-brother
  "r"          #'damer-refile
  "g"          #'damer-refresh
  "M-<left>"   #'damer-level-up
  "D"          #'damer-delete-items-and-descendants
  "TAB"        #'damer-toggle-expand)

(define-derived-mode damer-mode fundamental-mode "Damer"
  "Damer is a datamanager interface for managing items in emacs based on sqlite3."
  :global nil
  :group 'damer)

;;; Helper Functions
(defmacro damer--build-operator (name &rest fields)
  "Build getter and setters"
  (let ((index 0)
        (flen (length fields)))
    (defalias (intern (format "damer--%s-create" name))
      `(lambda ()
         (make-vector ,flen nil)))
    (dolist (e fields)
      (defalias (intern (format "damer--%s-set-%s" name e))
        `(lambda (vector newval)
           (aset vector ,index newval)
           newval))
      (defalias (intern (format "damer--%s-%s" name e))
        `(lambda (vector)
           (aref vector ,index)))
      (setq index (1+ index)))))

(defun damer-global-init ()
  "Globally initilize some things like OBJECT operators."
  (damer--build-operator item oid name category parent avail birth modify))

;;;###autoload
(defun damer (db-path)
  "Open a sqlite3 databse, and try to read from damer-tables or create them."
  (interactive "f")
  (let ((buffer (generate-new-buffer "datamanager")))
    (with-selected-window (selected-window)
      (switch-to-buffer buffer))
    (with-current-buffer buffer
      (damer-mode)
      (setq-local damer-database-path db-path)
      (damer--get-or-open-database)
      (damer--dao-try-create-tables)
      (damer-refresh)
      (setq-local buffer-read-only t)
      (goto-char (point-min)))))

;;;###autoload
(defun damer-quit ()
  "Quit current damer session"
  (interactive)
  (when (equal major-mode 'damer-mode)
    (kill-buffer)))

;;;###autoload
(defun damer-insert-child ()
  "Get current item, and insert a item as its child."
  (interactive)
  (damer-move-to-item-start)
  (let ((parent-oid (get-text-property (point) 'item-oid)))
    (damer-insert-item-1 parent-oid)))

;;;###autoload
(defun damer-insert-brother ()
  "Get current item, and insert a item as its brother.

We first get its parent, insert the new item as its parent's child,
thus this is a database first-class tool, so we could just insert the new
item into the database and refresh the ui."
  (interactive)
  (damer-move-to-item-start)
  (let ((parent-oid (get-text-property (point) 'item-parent)))
    (damer-insert-item-1 parent-oid)))

;;;###autoload
(defun damer-level-up ()
  "Move this item as its grandparent's child"
  (interactive)
  (damer-move-to-item-start)
  (let ((item-oid (get-text-property (point) 'item-oid)))
    (damer--dao-move-parent (damer--convert-pos-to-item))
    (damer-refresh)
    (damer--find-item item-oid)))

;;;###autoload
(defun damer-delete-items-and-descendants ()
  "Delete current and its descendants"
  (interactive)
  (damer-move-to-item-start)
  (let ((oid (get-text-property (point) 'item-oid))
        (name (get-text-property (point) 'item-name)))
    (when (yes-or-no-p (concat "Delete \`" name "' and its descenants?"))
      (damer--dao-delete-item-and-descendants oid)
      (damer-refresh))))

;;;###autoload
(defun damer-refresh ()
  (interactive)
  (let ((buffer-read-only nil))
    (erase-buffer)
    (mapcar #'delete-overlay (overlays-in (point-min) (point-max)))
    (insert (damer--convert-item-to-line (damer--dao-get-item "root")))
    (let ((list (damer--dao-get-descendants-sorted "root")))
      (dolist (e list)
        (damer--insert-item-line e)))))

;;;###autoload
(defun damer-toggle-expand ()
  (interactive)
  (let* ((end (1+ (damer--tree-end)))
         (start (line-beginning-position 2))
         (ovs (overlays-in start (1+ end)))
         (iovs (seq-filter (apply-partially
                            (lambda (ov)
                              (overlay-get ov 'damer-invisible)))
                           ovs)))
    (if (length> iovs 0)
        (dolist (e iovs)
          (delete-overlay e))
      (let ((ov (make-overlay start end)))
        (overlay-put ov 'damer-invisible t)
        (overlay-put ov 'invisible t)))))

(defun damer-refile ()
  (interactive)
  (damer-move-to-item-start)
  (let ((minibuffer-allow-text-properties t))
    (when-let* ((item (damer--convert-pos-to-item))
                (item-oid (damer--item-oid item))
                (all-items (damer--dao-get-all-items-for-total-list (list item-oid)))
                (option (completing-read "Refile:" all-items))
                (new-pid (with-temp-buffer
                           (insert option)
                           (goto-char (point-min))
                           (get-text-property (point) 'item-oid))))
      (damer--dao-move-other item new-pid)
      (damer-refresh)
      (damer--find-item item-oid))))

;;; UI
(defun damer-move-to-item-start ()
  (goto-char (line-beginning-position)))

(defun damer--find-item (oid)
  (let (item-point)
    (goto-char (point-min))
    (while (and (not (eobp)) (not item-point))
      (damer-move-to-item-start)
      (when (string= oid (get-text-property (point) 'item-oid))
        (setq item-point (point)))
      (forward-line))
    (if item-point
        (progn (goto-char item-point) item-point)
      nil)))

(defun damer--tree-end ()
  (damer-move-to-item-start)
  (let ((parent (get-text-property (point) 'item-parent))
        tree-end)
    (save-excursion
      (forward-line)
      (while (and (not tree-end)
                  (not (eobp)))
        (damer-move-to-item-start)
        (when (string= parent (get-text-property (point) 'item-parent))
          (previous-line)
          (setq tree-end (line-end-position)))
        (forward-line)))
    (if tree-end tree-end (point-max))))

(defun damer--insert-item-line (item)
  (when-let ((parent-point (damer--find-item (damer--item-parent item))))
    (goto-char (damer--tree-end))
    (setq buffer-read-only nil)
    (newline)
    (insert (damer--convert-item-to-line item))
    (let ((ov (make-overlay (line-beginning-position) (line-beginning-position))))
      (overlay-put ov 'before-string
                   (concat (make-string
                            (* 2 (damer--dao-depth (damer--item-oid item)))
                            ? )
                           (damer--get-item-icon (damer--item-category item))
                           " ")))
    (setq buffer-read-only t)))

(defun damer--select-category ()
  (let* ((categorys (mapcar #'car damer-categories))
         (option (completing-read "Item category: " categorys)))
    (if (member option categorys)
        (format "%s" option)
      (format "%s" (car categorys)))))

(defun damer-insert-item-1 (parent)
  (if-let* ((name (string-trim (read-string "Item Name: ")))
            (category (damer--select-category))
            (oid (md5 (format "%s%s%s%s" (emacs-uptime) category name parent)))
            (item (damer--build-item oid name category parent 1)))
      (progn
        (damer--dao-insert-item item)
        (damer--insert-item-line item))
    (message "Unformated Item")))

;;; Convert Functions
(defun damer--convert-item-to-line (item)
  ;; icon item_name
  (let ((category (damer--item-category item)))
    (propertize
     (damer--item-name item)
     'item-name (damer--item-name item)
     'item-category category
     'item-parent (damer--item-parent item)
     'item-oid (damer--item-oid item))))

(defun damer--get-item-icon (type)
  (if-let ((icon (assoc type damer-categories
                        (lambda (a b) (string= (format "%s" a) b)))))
      (cdr icon)
    "*"))

(defun damer--convert-pos-to-item ()
  (when (damer-move-to-item-start)
    (damer--build-item
     (get-text-property (point) 'item-oid)
     (get-text-property (point) 'item-name)
     (get-text-property (point) 'item-category)
     (get-text-property (point) 'item-parent)
     (get-text-property (point) 'item-avail))))

(defun damer--build-item (oid name category parent avail &optional birth modify)
  (let ((item (damer--item-create)))
    (damer--item-set-oid item oid)
    (damer--item-set-name item name)
    (damer--item-set-category item category)
    (damer--item-set-parent item parent)
    (damer--item-set-avail item avail)
    (damer--item-set-birth item birth)
    (damer--item-set-modify item modify)
    item))

;;; Dao
;;; Dao Helper Functions
(defun damer--dao-append-qmark (length)
  (string-join (make-vector length "?") ","))

(defun damer--dao-has-table (table)
  (sqlite-select (damer--get-or-open-database)
                 "SELECT name FROM sqlite_master WHERE type='table' AND name = ?;"
                 (list table)))

(defun damer--dao-try-create-tables ()
  (unless (damer--dao-has-table "items")
    (damer--dao-create-item-table)
    (damer--dao-insert-root)))

(defun damer--get-or-open-database ()
  (cond (damer-database damer-database)
        (damer-database-path
         (setq-local damer-database (sqlite-open damer-database-path))
         damer-database)
        (t (throw "Unable to open database" damer-database-path))))

(defun damer--dao-create-item-table ()
  (let ((table (concat "CREATE TABLE items ("
                       "oid            TEXT     PRIMARY KEY,"
                       "NAME           TEXT     NOT NULL,"
                       "category       TEXT     NOT NULL DEFAULT \"file\","
                       "parent         TEXT     default ROOT,"
                       "avail          integer  default 1,"
                       "birth          DATETIME DEFAULT CURRENT_TIMESTAMP,"
                       "modify         DATETIME DEFAULT CURRENT_TIMESTAMP,"
                       "UNIQUE(parent, name)"
                       ");" )))
    (sqlite-execute damer-database table)))

(defun damer--dao-depth (oid)
  (let ((sql "select parent from items where oid = ?")
        (depth 0))
    (while (not (string= oid "root"))
      (setq oid (caar (sqlite-select damer-database sql (list oid)))
            depth (1+ depth)))
    depth))

(defun damer--dao-insert-item (item)
  (let ((sql "INSERT INTO items (oid, name, category, parent, avail) VALUES (?, ?, ?, ?, ?)"))
    (sqlite-execute damer-database sql (list (damer--item-oid item)
                                             (damer--item-name item)
                                             (format "%s" (damer--item-category item))
                                             (damer--item-parent item)
                                             (damer--item-avail item)))))

(defun damer--dao-insert-root ()
  (let ((item (damer--build-item "root" "DATAMANAGER-ROOT" "folder" "rroot" 1)))
    (damer--dao-insert-item item)))

(defun damer--dao-sql-list-to-item (list)
  (let ((oid      (nth 0 list))
        (name     (nth 1 list))
        (category (nth 2 list))
        (parent   (nth 3 list))
        (avail    (nth 4 list))
        (birth    (nth 5 list))
        (modify   (nth 6 list)))
    (damer--build-item oid name category parent avail birth modify)))

(defun damer--dao-get-item (oid)
  (let ((sql "select oid, name, category, parent, avail, birth, modify from items where oid = ?"))
    (damer--dao-sql-list-to-item (car (sqlite-select damer-database sql (list oid))))))

(defun damer--dao-get-descendants (oids &optional unavailable)
  (let ((sql (concat "select oid, name, category, parent, avail, birth, modify "
                     "from items where parent in (%s) "
                     (if unavailable "" "and avail = 1 ")
                     "order by name asc"))
        result ret)
    (while oids
      (setq ret
            (sqlite-select
             damer-database
             (format sql (damer--dao-append-qmark (length oids)))
             oids))
      (setq result (append result ret))
      (setq oids (mapcar #'car ret)))
    (mapcar #'damer--dao-sql-list-to-item result)))

(defun damer--dao-delete-item-and-descendants (&rest oids &optional physical)
  (let* ((items (damer--dao-get-descendants oids))
         (to-delete-ids (if (and items (length> items 0))
                            (append (mapcar 'damer--item-oid items)
                                    (items))
                          oids))
         (delete-sql (format (if physical "delete from items where oid in (%s)"
                               "update items set avail = 0 where oid in (%s)")
                             (damer--dao-append-qmark (length to-delete-ids)))))
    (sqlite-execute damer-database delete-sql to-delete-ids)))

(defun damer--dao-move-parent (item)
  (when-let* ((parent (damer--item-parent item))
              (not-top (not (string= parent "root")))
              (pparent-sql "select parent from items where oid = ?")
              (pparent (caar (sqlite-select damer-database pparent-sql (list parent))))
              (update-sql "update items set parent = ? where oid = ?"))
    (sqlite-execute damer-database update-sql (list pparent (damer--item-oid item)))))

(defun damer--dao-move-other (item parent)
  (let* ((update-sql "update items set parent = ? where oid = ?"))
    (sqlite-execute damer-database update-sql (list parent (damer--item-oid item)))))

;;; Sort Functions
(defun damer--sort-items-by-tree (oid list)
  "

The list is relatively ordered.

That is to say:
  Parent Child1 Child2 Chile3 Child1_1 Child1_2 Child2_1 Child2_2."
  (let ((ht (make-hash-table :test 'equal))
        result
        todos)
    ;; First we build a hash-table to sort parent and children
    (dolist (e list)
      (let* ((parent (damer--item-parent e))
             (children (gethash parent ht)))
        (add-to-list 'children e t)
        (puthash parent children ht)))
    (setq todos (gethash oid ht))
    ;; Convert the hash table into a list
    (while (length> todos 0)
      (let* ((first (car todos))
             (others (cdr todos))
             (first-children (gethash (damer--item-oid first) ht)))
        (add-to-list 'result first t)
        (if (and first-children (length> first-children 0))
            (setq todos (append first-children others))
          (setq todos others))))
    result))

(defun damer--dao-get-descendants-sorted (oid)
  (let* ((result (damer--dao-get-descendants (list oid))))
    (damer--sort-items-by-tree oid result)))


(defun damer--dao-get-all-items-for-total-list (&optional exclude-oids)
  (let* ((oid "root")
         (list (damer--dao-get-descendants (list oid)))
         (ht (make-hash-table :test 'equal))
         (oid-ht (make-hash-table :test 'equal))
         result
         todos)
    ;; First we build a hash-table to sort parent and children
    (dolist (e list)
      (let* ((parent (damer--item-parent e))
             (children (gethash parent ht)))
        (add-to-list 'children e t)
        (puthash parent children ht)))
    (setq todos (gethash oid ht))
    ;; Convert the hash table into a list
    (while (length> todos 0)
      (let* ((first (car todos))
             (others (cdr todos))
             (first-children (gethash (damer--item-oid first) ht))
             (parent-oid (damer--item-parent first))
             (parent-name (gethash parent-oid oid-ht))
             (this-oid (damer--item-oid first))
             (this-name (damer--item-name first)))
        (unless (member this-oid exclude-oids)
          (cond ((string= "root" parent-oid)
                 (puthash this-oid (concat "/" this-name) oid-ht))
                ((length> parent-name 0)
                 (puthash this-oid (concat parent-name "/" this-name) oid-ht))))
        (if (and first-children (length> first-children 0))
            (setq todos (append first-children others))
          (setq todos others))))
    (maphash (lambda (key value)
               (push (propertize value 'item-oid key) result))
             oid-ht)
    result))

(provide 'damer)
