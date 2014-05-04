#!/usr/bin/sbcl --script
(load "~/.sbclrc")

(require :quicklisp)
(ql:quickload "cl-charms")

(load "~/.emacs.d/vendor/slime/swank-loader")
(swank-loader:init)

;; (require :swank)

(defvar *emacs-port* 4005)
(defparameter *arguments* (cdr *posix-argv*))
(defparameter *delimiters* '(" " "/" "\\"))

(defun start-swank ()
  (setf swank:*configure-emacs-indentation* nil
        swank::*enable-event-history* nil
        swank:*log-events* t)
  (swank:create-server :port *emacs-port* :dont-close t))

(defclass query ()
  ((search-string :initform ""
                  :reader search-string)
   (changedp :initform nil
             :reader changedp)))

(defgeneric update-search-string (query new-string)
  (:documentation "Update the 'search-string' on a query object"))

(defmethod update-search-string ((query-instance query) new-string)
  (let ((previous (slot-value query-instance 'search-string)))
    (unless (string= previous new-string)
      (setf (slot-value query-instance 'search-string) new-string)
      (setf (slot-value query-instance 'changedp) t))))

(defparameter *results-list* '("some/result/one" "some/result/two"))

(defparameter *cursor-y* 0)

(defun concat-list (list &optional spacer)
  (format nil (concatenate 'string "~{~a~^" spacer "~}") list))

(defun string-cut-end (string &key keep cut)
  (cond
    (keep (subseq string 0 (- keep 1)))
    (cut (subseq string 0 (- (length string) cut)))))

(defun insert-into (type string sub pos)
  (let ((end (length string)))
    (concatenate type (subseq string 0 pos) sub (subseq string pos end))))

(defun kill-string-from (pos string)
  (subseq string 0 pos))

(defun kill-query-string-from (pos query-object)
  (let ((string (search-string query-object)))
    (update-search-string query-object (kill-string-from pos string))))

(defun position-of-nearest-delim-between-0-and-point (pos query-object)
  (let* ((string (search-string query-object))
        (subsequence (subseq string 0 pos)))
    (apply #'max (mapcar (lambda (delim)
                           (or (position delim subsequence :from-end t :test #'string=)
                               0))
                         *delimiters*))))

(defun position-of-nearest-delim-between-point-and-end (pos query-object)
  (let ((string (search-string query-object)))
    (if  (< pos (length string))
         (let ((subsequence (subseq string (+ 1 pos))))
           (+ 1
              pos
              (apply #'min
                     (mapcar (lambda (delim)
                               (or (position delim subsequence :test #'string=)
                                   (length subsequence)))
                             *delimiters*))))
         pos)))

(defun move-cur-y (pos)
  (setf *cursor-y* pos))

(defun move-backwards-to-delim (pos query-object)
  (move-cur-y (position-of-nearest-delim-between-0-and-point pos query-object)))

(defun move-forwards-to-delim (pos query-object)
  (move-cur-y (position-of-nearest-delim-between-point-and-end pos query-object)))

(defun kill-query-string-word-backwards (pos query-object)
  (let ((point (position-of-nearest-delim-between-0-and-point pos query-object))
        (string (search-string query-object)))
    (update-search-string query-object
                          (concatenate 'string
                            (subseq string 0 point)
                            (subseq string pos)))
    (move-cur-y point)))

(defun kill-query-string-character-from (pos query-object)
  (let ((string (search-string query-object)))
    (unless (= pos (length string))
      (update-search-string query-object
                            (concatenate 'string
                              (subseq string 0 pos)
                              (subseq string (+ pos 1) (length string)))))))


(defun cur-y ()
  *cursor-y*)

(defun inc-cur-y (max &optional num)
  (if (< *cursor-y* max)
      (setf *cursor-y* (+ *cursor-y* (or num 1)))))

(defun dec-cur-y (&optional num)
  (if (>= (- *cursor-y* (max 1 (or num 0))) 0)
      (setf *cursor-y* (- *cursor-y* (or num 1)))))


(defun move-cursor (win)
  (cl-charms:wmove win 0 (cur-y)))

(defun make-query-window ()
  (let ((win (cl-charms:newwin 1 (- cl-charms:*COLS* 15) 2 10)))
    win))

(defun make-results-window ()
  (let ((win (cl-charms:newwin (- cl-charms:*LINES* 4) cl-charms:*COLS* 4 0)))
    (cl-charms:scrollok win (cffi:convert-to-foreign t :boolean))
    win))

(defun draw-border (win)
  (cl-charms:box win (char-code (char "#" 0)) (char-code (char "#" 0))))

(defun write-headline (win)
  (let ((title "Filing system fuzzy search awesome!"))
    (cl-charms:wmove win 1 (floor  (/ (- cl-charms:*COLS* (length title)) 2)))
    (cl-charms:waddstr win title)))

(defun write-cmd-prompt (win)
  (cl-charms:wmove win 2 3)
  (cl-charms:waddstr win "query: "))

(defun write-query-string (win string)
  (when (> (length string) 0)
    (cl-charms:wmove win 0 0)
    (cl-charms:waddstr win string)))

(defun clear-windows (&rest windows)
  (loop
     for win in windows
     do (cl-charms:werase win)))

(defun write-results (win)
 (loop
    for i from 0 to (- (length *results-list*) 1)
    do (progn
         (cl-charms:wmove win (+ i 1) 2)
         (cl-charms:waddstr win (elt *results-list* i)))))

(defun process-query (query-object)
  (when (changedp query-object)
    (setf *results-list* (list (search-string query-object)))
    (find-matches (search-string query-object))))

(defun find-matches (search-string) nil)
(defun next-match () nil)

(defun previous-match () nil)

(defun auto-fill-query-string () nil)

(defun select-match () nil)

(defun draw-everything (query-object main-window query-window results-window)
  (clear-windows main-window results-window query-window)
  (draw-border main-window)
  (draw-border results-window)
  (write-headline main-window)
  (write-cmd-prompt main-window)
  (write-query-string query-window (search-string query-object))
  (write-results results-window)
  (move-cursor query-window)
  (cl-charms:wrefresh main-window)
  (cl-charms:wrefresh results-window)
  (cl-charms:wrefresh query-window))

(defun grab-input-char ()
  (cl-charms:noecho)
  (let ((charcode (cl-charms:getch)))
    (cl-charms:echo)
    (code-char charcode)))

(defun use-input-char (input query-object)
  (let ((previous-search (search-string query-object)))
    (cond
      ((or (char= input #\Backspace) (char= input #\Rubout))
       (if (> (length (search-string query-object)) 0)
           (progn
             (update-search-string query-object (string-cut-end previous-search :cut 1 ))
             (dec-cur-y))))

      ((or (char= input #\Newline) (char= input #\Return)) (select-match))

      ((char= input #\Esc)
       (let ((unput (grab-input-char)))
         (cond
           ;; M-f
           ((char= unput #\f) (move-forwards-to-delim (cur-y) query-object))
           ;; M-b
           ((char= unput #\b) (move-backwards-to-delim (cur-y) query-object)))))
      ;; C-a
      ((char= input #\Soh) (move-cur-y 0))
      ;; C-e
      ((char= input #\Enq) (move-cur-y (length (search-string query-object))))
      ;; C-k
      ((char= input #\Vt) (kill-query-string-from (cur-y) query-object))
      ;; C-d
      ((char= input #\Eot) (kill-query-string-character-from (cur-y) query-object))
      ;; C-w
      ((char= input #\Etb) (kill-query-string-word-backwards (cur-y) query-object))
      ;; C-l
      ((char= input #\Page) (cl-charms:wclear *screen*))
      ;; Tab
      ((char= input #\Tab) (auto-fill-query-string))
      ;; C-s
      ((char= input #\Dc3) (next-match))
      ;; C-r
      ((char= input #\Dc2) (previous-match))
      ;; C-f
      ((char= input #\Ack) (inc-cur-y (length (search-string query-object))))
      ;; C-b
      ((char= input #\Stx) (dec-cur-y))
      ;; C-q
      ((char= input #\Dc1)
       (cl-charms:endwin)
       (quit))
      ;; Finally
      (t
       (progn
         (update-search-string query-object (insert-into 'string previous-search (format nil "~s" swank:*global-debugger*) (cur-y)))
         ;; (update-search-string query-object (insert-into 'string previous-search (format nil "~a" input) (cur-y)))
         (inc-cur-y (length (search-string query-object))
                    (length (string input))))))))

(defun poll-for-input (query-object)
  (use-input-char (grab-input-char) query-object))

(defun draw-everything-emacs (&rest rest) nil)
(defun poll-for-input-emacs (query-object)
  (use-input-char #\a query-object))

(defun main ()

  (defparameter *screen* (cl-charms:initscr))
  (defparameter *query-window* (make-query-window))
  (defparameter *results-window* (make-results-window))
  (defparameter *query-object* (make-instance 'query))

  (let ((screen *screen*)
        (query-window *query-window*)
        (results-window *results-window*)
        (query-object *query-object*))
    (loop while t do (progn
                       (draw-everything query-object screen query-window results-window)
                       (poll-for-input query-object)
                       (process-query query-object))
       finally (cl-charms:endwin))))

(let ((swank::*loopback-interface* "127.0.0.1") (port 4006)) (swank:create-server :port port ))
(setf sb-ext:*invoke-debugger-hook* (or *debugger-hook*))
(main)

;; (handler-case
;;     (main)
;;   (condition (se)
;;     (progn
;;       (cl-charms:endwin)
;;       (start-swank))))
