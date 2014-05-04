(ql:quickload "cl-ppcre")
(ql:quickload "cl-fad")
(ql:quickload "cl-utilities")

(defclass match nil
  ((value :initform ""
          :accessor value
          :documentation "The string that matched the query")
   (correlation :initform 0
                :accessor correlation
                :documentation "How correlated the match is to the original query, low numbers = more correlated"))
  (:documentation "match type for use when matching a string/regex against another string."))

(defun collate-matches (query-terms items)
  "Call match on each query-term and the list of items, collecting the results into a list"

  (loop for term in query-terms
     collecting (match term items)))

(defun get-dir-path-start-end (query-string)
  "Return the expanded directory path from query-string."
  (let ((regex "^[\/(\~\/)](.*[\/])?")) ; match / or ~/,  then any chars sans space and finally / 0 or 1 times
    (cl-ppcre:scan regex query-string)))

(defun get-dir-path (query-string)
  "Return the expanded directory path from query-string."
  (multiple-value-bind (start end) (get-dir-path-start-end query-string)
    (if (and start end)
        (cl-fad:pathname-as-directory (subseq query-string start end))
      (cl-fad:pathname-as-directory (truename ".")))))

(defun get-search-terms (query-string)
  "Return a list of search terms from query string.

Returns every word after the last / in query string, using spaces as delimiters."
  (let ((terms-string))

    (multiple-value-bind (start end) (get-dir-path-start-end query-string)
      (if (and start end)
          (setf terms-string (subseq query-string end (length query-string)))
        (setf terms-string query-string)))
    (loop for str in (cl-utilities:split-sequence #\Space terms-string)
       when (> (length str) 0) collect str)))



(defun match (query items)
  "Return a list of match objects representing each element in the list ITEMS that matches the string QUERY.

One or more matcher functions may be used, and a correlation is given to each match"
  (list "example/match.file" "other/example/match.file"))

(defun get-nth-items (path &optional level)
  "Recursively return all items from path, descending LEVEL directories deep.

If LEVEL is nil, do not recurse"

  nil)

(defun sort-matches (matches)
  "Sort matches by their correlation primarily string value secondarily"

  matches)

(defun match-dir-path (path match)
  "Return the full directory path to the MATCH item found under PATH.

Example, for path /home/username/ and matching somedir/somefile.file
=> /home/username/somedir/"

  nil)

(defun match-full-path (path match)
  "Return the full path to the MATCH item found under PATH.
Example, for path /home/username and matching somedir/somefile.file
=> /home/username/somedir/somefile.file"

  nil)
