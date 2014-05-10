(in-package :sick-filing)

(defparameter *cursor-y* 0)

(defun make-query-window ()
  (let ((win (cl-charms:newwin 1 (- cl-charms:*COLS* 15) 2 10)))
    win))

(defun make-results-window ()
  (let ((win (cl-charms:newwin (- cl-charms:*LINES* 4) cl-charms:*COLS* 4 0)))
    (cl-charms:scrollok win (cffi:convert-to-foreign t :boolean))
    win))

(defun draw-border (win)
  (cl-charms:box win (char-code (char "#" 0)) (char-code (char "#" 0))))

(defun write-headline (win &optional string query-string)
  (let ((title  (concatenate 'string  string  " - Filing system fuzzy search awesome! - " (write-to-string (length query-string)))))
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

(defun write-results (win results-instance)
  (let ((list (results-list results-instance)))
    (loop
       for i from 0 to (- (length list) 1)
       do (progn
            (cl-charms:wmove win (+ i 1) 2)
            (cl-charms:waddstr win (elt list i))))))

(defun draw-everything (results-instance query-object main-window query-window results-window)
  (clear-windows main-window results-window query-window)
  (draw-border main-window)
  (draw-border results-window)
  (write-headline main-window (namestring (parse-dir-path (search-string query-object))) (search-string query-object))
  (write-cmd-prompt main-window)
  (write-query-string query-window (search-string query-object))
  (write-results results-window results-instance)
  (move-cursor query-window)
  (cl-charms:wrefresh main-window)
  (cl-charms:wrefresh results-window)
  (cl-charms:wrefresh query-window))

(defun grab-input-char ()
  (cl-charms:noecho)
  (let ((charcode (cl-charms:getch)))
    (cl-charms:echo)
    (code-char charcode)))

(defun clean-up-and-quit ()
  (cl-charms:endwin)
  (cl-user::quit))

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

(defun move-cur-y (pos)
  (setf *cursor-y* pos))
