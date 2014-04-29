(cl:in-package :sick-filing)

(defparameter *query-string* "")
(defparameter *results-list* '("some/result/one" "some/result/two"))
(defparameter *screen* (cl-charms:initscr))

(defun string-cut-end (string &key keep cut)
  (cond
    (keep (subseq string 0 (- keep 1)))
    (cut (subseq string 0 (- (length string) cut)))))

(defun initialize () )

(defun make-main-window ()
  (cl-charms:newwin cl-charms:*LINES* cl-charms:*COLS* 0 0))

(defun make-query-window ()
  (let ((win (cl-charms:newwin 1 (- cl-charms:*COLS* 15) 2 10)))
    ;; (cl-charms:init_pair 1 cl-charms:COLOR_BLUE cl-charms:COLOR_MAGENTA)
    ;; (cl-charms:wbkgd win (cl-charms:color_pair 1))
    win))

(defun make-results-window ()
  (cl-charms:newwin (- cl-charms:*LINES* 5) cl-charms:*COLS* 5 0))

(defun draw-border (win)
  (cl-charms:box win (char-code (char "#" 0)) (char-code (char "#" 0))))

(defun write-headline (win)
  (let ((title "Filing system fuzzy search awesome!"))
    (cl-charms:wmove win 1 (/ (- cl-charms:*COLS* (length title)) 2))
    (cl-charms:waddstr win title)))


(defun write-cmd-prompt (win)
  (cl-charms:wmove win 2 3)
  (cl-charms:waddstr win "query: "))

(defun write-query-string (win)
  (when (> (length *query-string*) 0)
    (cl-charms:wmove win 0 0)
    (cl-charms:waddstr win *query-string*)))

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

(defun process-query ()
  (setf *results-list*
        (stable-sort (remove-duplicates (concatenate 'list *results-list* `(,*query-string*))
                                        :test #'string=)
                     #'string<)))

(defun draw-everything (main-window query-window results-window)
  (clear-windows results-window query-window)
  (draw-border main-window)
  (draw-border results-window)
  (write-headline main-window)
  (write-cmd-prompt main-window)
  (write-query-string query-window)
  (write-results results-window)
  (cl-charms:wrefresh main-window)
  (cl-charms:wrefresh results-window)
  (cl-charms:wrefresh query-window))

(defun grab-input-char ()
  (cl-charms:noecho)
  (let ((charcode (cl-charms:getch)))
    (cl-charms:echo)
    (code-char charcode)))

(defun use-input-char (input)
  (cond
    ((or (char= input #\Backspace)
         (char= input #\Rubout))
     (if (> (length *query-string*) 0)
         (setf *query-string* (string-cut-end *query-string* :cut 1 ))))

    ((or (char= input #\Newline)
         (char= input #\Return))
     nil)

    (t (setf *query-string* (concatenate 'string *query-string* (string input))))))

(defun poll-for-input ()
  (use-input-char (grab-input-char)))

(defun main ()
  (initialize)
  (let ((main-window (make-main-window))
        (query-window (make-query-window))
        (results-window (make-results-window)))
    (loop while t do (progn
                       (draw-everything main-window query-window results-window)
                       (poll-for-input)
                       (process-query))
       finally (cl-charms:endwin))))
