(in-package :sick-filing)

(defparameter *delimiters* '(" " "/" "\\"))
(defparameter *results-list* nil)

(defclass query ()
  ((search-string :initform ""
                  :reader search-string)
   (changedp :initform nil
             :reader changedp)))

;; (defgeneric update-search-string (query new-string)
;;   (:documentation "Update the 'search-string' on a query object"))

(defmethod update-search-string ((query-instance query) new-string)
  (let ((previous (slot-value query-instance 'search-string)))
    (unless (string= previous new-string)
      (setf (slot-value query-instance 'search-string) new-string)
      (setf (slot-value query-instance 'changedp) t))))

(defclass results ()
  ((result-list :accessor result-list)
   (index :accessor index)))

(defmethod current-result ((result-instance))
  (nth (index result-instance) (result-list result-instance)))

(defmethod next-result ((result-instance result))
  (let ((index (+ (index result-instance) 1)))
    (unless (> index (length (result-list result-instance)))
      (setf (index result-instance) index)
      (nth index (result-list result-instance)))))

(defmethod previous-result ((result-instance result))
  (let ((index (- (index result-instance) 1)))
    (unless (< index 0)
      (setf (index result-instance) index)
      (nth index (result-list result-instance)))))



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

(defun process-query (query-object)
  (when (changedp query-object)
    (setf *results-list* (list (search-string query-object)))
    (find-matches (search-string query-object))))

(defun poll-for-input (query-object)
  (use-input-char (grab-input-char) query-object))

(defun find-matches (search-string) nil)

(defun next-match () nil)

(defun previous-match () nil)

(defun auto-fill-query-string () nil)

(defun select-match () nil)

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
      ;; C-f
      ((char= input #\Ack) (inc-cur-y (length (search-string query-object))))
      ;; C-b
      ((char= input #\Stx) (dec-cur-y))
      ;; C-q
      ((char= input #\Dc1)
       (clean-up-and-quit))
      ;; C-s
      ((char= input #\Dc3) (next-match))
      ;; C-r
      ((char= input #\Dc2) (previous-match))
      ;; Tab
      ((char= input #\Tab) (auto-fill-query-string))
      ;; Finally
      (t
       (progn
         ;; (update-search-string query-object (insert-into 'string previous-search (format nil "~s" input) (cur-y)))
         (update-search-string query-object (insert-into 'string previous-search (format nil "~a" input) (cur-y)))
         (inc-cur-y (length (search-string query-object))
                    (length (string input)))
         (process-query query-object))))))

(defun main-1 (argv)
  (let ((swank::*loopback-interface* "127.0.0.1") (port 4006))
    (swank-loader:init)
    (swank:create-server :port port ))

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
                       (poll-for-input query-object))
       finally (cl-charms:endwin))))


(defun main (argv)
  (handler-case (main-1 argv)
    (sb-sys:interactive-interrupt ()
      (clean-up-and-quit))))
