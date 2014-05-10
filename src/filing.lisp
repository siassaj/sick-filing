(in-package :sick-filing)

(defparameter *delimiters* '(" " "/" "\\"))

(defgeneric update-search-string (query new-string)
  (:documentation "Update the 'search-string' on a query object"))

(defmethod update-search-string ((query-instance query) new-string)
  (let ((previous (slot-value query-instance 'search-string)))
    (unless (string= previous new-string)
      (setf (slot-value query-instance 'search-string) new-string)
      (setf (slot-value query-instance 'changedp) t))))

(defgeneric rotate-left (object))

(defgeneric rotate-right (object))

(defmethod rotate-left ((results-instance results))
  (let* ((list (results-list results-instance))
        (first (car list)))
    (setf (results-list results-instance) (concatenate 'list (cdr list) (list first)))))

(defmethod rotate-right ((results-instance results))
  (let* ((list (results-list results-instance))
         (last (last list))
         (rest (subseq list 0 (- (length list) 1))))
    (setf (results-list results-instance) (concatenate 'list last rest))))

(defun next-match (results-instance)
  (rotate-left results-instance))

(defun previous-match (results-instance)
  (rotate-right results-instance))

(defun kill-query-string-from (pos query-object)
  (let ((string (search-string query-object)))
    (update-search-string query-object (kill-string-from pos string))))

(defun position-of-nearest-delim-between-0-and-point (pos query-object)
  (let* ((string (search-string query-object))
        (subsequence (subseq string 0 (max (- pos 1) 0))))
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
  (let ((point (+ 1  (position-of-nearest-delim-between-0-and-point pos query-object)))
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

(defun process-query (query-object results-instance)
  (when (changedp query-object)
    (setf (slot-value query-object 'changedp) nil)
    (multiple-value-bind (matches dir current-depth num-dirs) (do-query (search-string query-object) 2)
      (setf (results-list results-instance)
            (sort-matches matches)))))


(defun poll-for-input (results-instance query-object)
  (use-input-char (grab-input-char) results-instance query-object))

(defun auto-fill-query-string (query-object results-instance)
  (let ((list (results-list results-instance))
        item
        pathname)
    (when (>  (length list) 0)
      (setf item (nth 0 list))
      (setf pathname (path item))

      (when (cl-fad:directory-pathname-p pathname)
        (update-search-string query-object (namestring pathname))
        (set-cur-y (length (namestring pathname))))

      (unless (string=
               (search-string query-object)
               (namestring pathname))
        (next-match results-instance)))))

(defun select-match (results-instance) nil)

(defun use-input-char (input results-instance query-object)
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
      ((char= input #\Dc3) (next-match results-instance))
      ;; C-r
      ((char= input #\Dc2) (previous-match results-instance))
      ;; Tab
      ((char= input #\Tab) (auto-fill-query-string query-object results-instance))
      ;; Finally
      (t
       (progn
         (update-search-string query-object (insert-into 'string previous-search (format nil "~a" input) (cur-y)))
         (inc-cur-y (length (search-string query-object))
                    (length (string input))))))))

(defun main-1 (argv)
  (let ((swank::*loopback-interface* "127.0.0.1") (port 4005))
    ;; (swank-loader:init)
    (swank:create-server :port port ))

  (defparameter *screen* (cl-charms:initscr))
  (defparameter *query-window* (make-query-window))
  (defparameter *results-window* (make-results-window))
  (defparameter *query-object* (make-instance 'query))
  (defparameter *results-instance* (make-instance 'results))

  (let ((screen *screen*)
        (query-window *query-window*)
        (results-window *results-window*)
        (query-object *query-object*)
        (results-instance *results-instance*))

    (loop while t do (progn
                       (process-query query-object results-instance)
                       (draw-everything results-instance query-object screen query-window results-window)
                       (poll-for-input results-instance query-object))
       finally (cl-charms:endwin))))

(defun main (argv)
  (handler-case (main-1 argv)
    (sb-sys:interactive-interrupt ()
      (clean-up-and-quit))))

;; (defun main (argv)
;;   (handler-case (main-1 argv)
;;     (condition (se)
;;       (cl-charms:endwin)
;;       (format t "~s~%" se))))
