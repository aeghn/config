;;; Icons
;;; stealed from https://github.com/casouri/lunarymacs/blob/f01edd6b148a1ce89e3cbad90a2ed6fd015e8279/star/deprecated/tool-bar.el

(add-to-list
 'image-load-path
 (expand-file-name "lib/images" (file-name-directory (chin/this-true-file))))

;;; Global map

;; Control the sequence of icons.
(setq tool-bar-map (make-sparse-keymap))

(define-key tool-bar-map [mdired]
            `(menu-item "Open Directory..." mdired
                        :enable (menu-bar-non-minibuffer-window-p)
                        :help "Read a directory, to operate on its files"
                        :image ,(find-image '((:type svg :file "mdired.svg")))))

(define-key tool-bar-map [magit]
            `(menu-item "Magit" magit
                        :enable t
                        :help "Start Magit "
                        :image ,(find-image '((:type svg :file "magit.svg")))))
(define-key tool-bar-map [separator-4] '("--"))


(define-key tool-bar-map [isearch-forward]
            `(menu-item "Forward String..." isearch-forward
                        :enable t
                        :help "Search forward for a string as you type it"
                        :image ,(find-image '((:type svg :file "tm_search.svg")))))
(define-key tool-bar-map [separator-3] '("--"))

(define-key tool-bar-map [paste]
            `(menu-item "Paste" yank
                        :enable t
                        :help "Paste (yank) text most recently cut/copied"
                        :image ,(find-image '((:type svg :file "tm_paste.svg")))))

(define-key tool-bar-map [copy]
            `(menu-item "Copy" ns-copy-including-secondary
                        :enable mark-active
                        :help "Copy text in region between mark and current position"
                        :image ,(find-image '((:type svg :file "tm_copy.svg")))))

(define-key tool-bar-map [cut]
            `(menu-item "Cut" kill-region
                        :enable (and mark-active (not buffer-read-only))
                        :help "Cut (kill) text in region between mark and current position"
                        :image ,(find-image '((:type svg :file "tm_cut.svg")))))

(define-key tool-bar-map [separator-1] '("--"))

(define-key tool-bar-map [undo]
            `(menu-item "Undo" undo
                        :enable (and (not buffer-read-only)
                                     (not (eq t buffer-undo-list))
                                     (if (eq last-command 'undo)
                                         (listp pending-undo-list)
                                       (consp buffer-undo-list)))
                        :help "Undo last edits"
                        :image ,(find-image '((:type svg :file "tm_undo.svg")))))

(define-key tool-bar-map [buffer-read-only]
            `(menu-item "Read Only Mode" read-only-mode
                        :enable t
                        :help "Buffer read only mode?"
                        :image
                        ,(find-image '((:type svg :file
                                              ,(if buffer-read-only "tm_lock_open.svg" "tm_lock.svg"))))))

(define-key tool-bar-map [separator-2] '("--"))

(define-key tool-bar-map [save-buffer]
            `(menu-item "Save" save-buffer
                        :enable (and (buffer-modified-p)
                                     (buffer-file-name)
                                     (menu-bar-non-minibuffer-window-p))
                        :help "Save current buffer to its file"
                        :image ,(find-image '((:type svg :file "tm_save.svg")))))


(define-key tool-bar-map [dired]
            `(menu-item "Open Directory..." dired
                        :enable (menu-bar-non-minibuffer-window-p)
                        :help "Read a directory, to operate on its files"
                        :image ,(find-image '((:type svg :file "diropen.svg")))))

(define-key tool-bar-map [kill-buffer]
            `(menu-item "Close" kill-this-buffer
                        :enable (kill-this-buffer-enabled-p)
                        :help "Discard (kill) current buffer"
                        :image ,(find-image '((:type svg :file "tm_cross.svg")))))

(define-key tool-bar-map [open-file]
            `(menu-item "Open File..." menu-find-file-existing
                        :enable (menu-bar-non-minibuffer-window-p)
                        :help "Read an existing file into an Emacs buffer"
                        :image ,(find-image '((:type svg :file "tm_open.svg")))))

(define-key tool-bar-map [new-file]
            `(menu-item "Visit New File..." find-file
                        :enable (menu-bar-non-minibuffer-window-p)
                        :help "Specify a new file's name, to edit the file"
                        :image ,(find-image '((:type svg :file "tm_new.svg")))))

;;; Isearch

(setq isearch-tool-bar-map (make-sparse-keymap))

(define-key isearch-tool-bar-map [isearch-describe-mode]
            `(menu-item "Help" isearch-describe-mode
                        :help "Get help for Isearch"
                        :image ,(find-image '((:type svg :file "tm_info.svg")))))


(define-key isearch-tool-bar-map [isearch-cancel]
            `(menu-item "Abort" isearch-cancel
                        :help "Abort search"
                        :image ,(find-image '((:type svg :file "tm_cross.svg")))))

(define-key isearch-tool-bar-map [isearch-exit]
            `(menu-item "Finish" isearch-exit
                        :help "Finish search leaving point where it is"
                        :image ,(find-image '((:type svg :file "tm_exit.svg")))))


(define-key isearch-tool-bar-map [isearch-query-replace]
            `(menu-item "Replace" isearch-query-replace
                        :help "Replace search string"
                        :image ,(find-image '((:type svg :file "tm_search-replace.svg")))))

(define-key isearch-tool-bar-map [isearch-occur]
            `(menu-item "Occur" isearch-occur
                        :help "Show each search hit"
                        :image ,(find-image '((:type svg :file "tm_index.svg")))))

(define-key isearch-tool-bar-map [isearch-repeat-backward]
            `(menu-item "Repeat backward" isearch-repeat-backward
                        :help "Repeat search backward"
                        :image ,(find-image '((:type svg :file "tm_back-arrow.svg")))))

(define-key isearch-tool-bar-map [isearch-repeat-forward]
            `(menu-item "Repeat forward" isearch-repeat-forward
                        :help "Repeat search forward"
                        :image ,(find-image '((:type svg :file "tm_fwd-arrow.svg")))))


;;; Help Mode
(setq help-mode-tool-bar-map
      (let ((map (make-sparse-keymap)))
         (require 'help-mode)
         (tool-bar-local-item "close" 'quit-window 'quit map
                              :help "Quit help"
                              :vert-only t
                              :image (find-image '((:type svg :file "tm_exit.svg"))))
         (define-key-after map [separator-8] menu-bar-separator)
         (tool-bar-local-item "search" 'isearch-forward 'search map
                              :help "Search" :vert-only t
                              :image (find-image '((:type svg :file "tm_search.svg"))))
         (tool-bar-local-item-from-menu 'help-go-back "left-arrow" map help-mode-map
                                        :rtl "right-arrow" :vert-only t
                                        :image (find-image '((:type svg :file "tm_back-arrow.svg"))))
         (tool-bar-local-item-from-menu 'help-go-forward "right-arrow" map help-mode-map
                                        :rtl "left-arrow" :vert-only t
                                        :image (find-image '((:type svg :file "tm_fwd-arrow.svg"))))
         map))


