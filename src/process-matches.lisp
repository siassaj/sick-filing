(in-package :sick-filing)

(defun dir-path-start-end (query-string)
  "Return the expanded directory path from query-string."
  (let ((regex "^[\/(\~\/)](.*[\/])?")) ; match / or ~/,  then any chars sans space and finally / 0 or 1 times
    (cl-ppcre:scan regex query-string)))

(defun parse-dir-path (query-string)
  "Return the expanded directory path from query-string."
  (multiple-value-bind (start end) (dir-path-start-end query-string)
    (if (and start end)
        (cl-fad:pathname-as-directory (subseq query-string start end))
      (cl-fad:pathname-as-directory (truename ".")))))

(defun parse-query-terms (query-string)
  "Return a list of search terms from query string.

Returns every word after the last / in query string, using spaces as delimiters."
  (let ((terms-string))

    (multiple-value-bind (start end) (dir-path-start-end query-string)
      (if (and start end)
          (setf terms-string (subseq query-string end (length query-string)))
        (setf terms-string query-string)))
    (loop for str in (cl-utilities:split-sequence #\Space terms-string)
       when (> (length str) 0) collect str)))

(defun strengthen-correlation (item &optional amount)
  (setf (correlation item) (* (+ 1 amount) (correlation item))))

(defun match-items (query items)
  "Return a list of match objects representing each element in the list ITEMS that matches the string QUERY.

One or more matcher functions may be used, and a correlation is given to each match"
   (let ((prefix-regex (format nil "(?i)^~a.*" query))
         (full-regex (format nil "(?i).*~a.*" query))
         (fuzzy-regex "")
         matches)
     (setf fuzzy-regex (concatenate 'string fuzzy-regex "(?i).*"))
     (loop for character across query
        counting t into counter
        do (setf fuzzy-regex (concatenate 'string fuzzy-regex
                                          (if (= counter (length query))
                                              (format nil "~a" character)
                                              (format nil "~a.*?" character)))))
   (loop for item in items
      do (cond
           ((cl-ppcre:scan prefix-regex (relative-path item))
            (strengthen-correlation item 1)
            (push item matches))
           ((cl-ppcre:scan full-regex (relative-path item))
            (strengthen-correlation item 0.5)
            (push item matches))
           ((cl-ppcre:scan fuzzy-regex (relative-path item))
            (strengthen-correlation item 0.25)
            (push item matches))))
   matches))

(defun collate-matching-items (query-terms items)
  "Call match on each query-term and the list of items, collecting the results into a list"
  (let ((matches '()))
    (if query-terms
        (loop for term in query-terms
           counting term into i
           do (let ((term-matches (match-items term items)))
                 (dolist (match term-matches)
                   (setf (correlation match) (* (+ 1 i) (correlation match))))
                 (setf matches (concatenate 'list term-matches matches)))             )
      (setf matches  (concatenate 'list items matches)))
    (remove-duplicates matches)))

(defun build-items (dir raw-paths)
  (loop for path in raw-paths
     collect (make-instance 'item
                            :path path
                            :relative-path (cl-ppcre:regex-replace
                                            (format nil "^~a" (namestring dir))
                                            (namestring path)
                                            ""))))

(defun match-directory (dir query-terms)
  "Match all the contents of a given directory and return list of matches and new directories to go down"
  (let ((raw-paths (cl-fad:list-directory dir :follow-symlinks nil))
         processed-items matches directories)

    (setf processed-items (build-items dir raw-paths))
    (setf matches (collate-matching-items query-terms processed-items))
    (setf directories (remove-if-not #'cl-fad:directory-pathname-p raw-paths))

    (values matches directories)))

;; LEXICAL CLOSURE ENVIRONMENT FOR DO-QUERY
(let ((current-query-string "")
      (current-depth 0)
      directory-stack new-directory-stack query-terms)

  (defun do-query (query-string max-depth &key clear)
    "Find items in the file system matching the structured QUERY-STRING.

This function exists in a closure, and if the query-string does not change, it will create a directory stack.

Each time it's called it will return multiple values MATCHES, DIR, CURRENT-DEPTH and NUM-DIRS; or nil if CURRENT-DEPTH exceeds MAX-DEPTH from the next directory in the directory stack. When the directory stack runs out, it will descend further down another level in the directory tree.

When QUERY-STRING changes it will reset it's directory stack and begin from the top again.

MATCHES is an array of mathing Item instances
DIR is the current directory, since it may have entered a new directory to match items
CURRENT-DEPTH is how many levels deeper than the initial directory depth.
NUM-DIRS is the number of directories left to search at the current tree depth"
    (cond
      ;; Reset the lexial closure environment if the query changes
      ((or clear
           (not (string= query-string current-query-string)))
       (setf query-terms (parse-query-terms query-string))
       (setf current-query-string query-string)
       (setf current-depth 0)
       (setf directory-stack nil)
       (setf new-directory-stack nil)))
    ;; Return nil if we've gone too deep
    (when (> current-depth max-depth) (return-from do-query))
    ;; Return nil if we've looked at our primary directory and there are no
    ;; deeper directories in the tree
    (when (and (= 1 current-depth)
               (= 0 (length new-directory-stack))
               (= 0 (length directory-stack)))
      (return-from do-query))
    ;; Get matches for either the next directory on the stack, or the primary tree directory
    ;; the return-depth and dir-stack-length bindings are created so that we can return those
    ;; values after the directory-stack and current-depth are changed to begin walking the
    ;; next layer down.
    (let ((dir (or (pop directory-stack)
                   (parse-dir-path query-string)))
          (return-depth current-depth)
          (dir-stack-length (length directory-stack)))
      (multiple-value-bind (matches directories) (match-directory dir query-terms)
        (setf new-directory-stack (concatenate 'list new-directory-stack directories))
        ;; Increment depth and replace directory stack if it runs out)
        ;; This is done here because we populated new-directory-stack just a few forms ago
        (when (= 0 dir-stack-length)
          (setf directory-stack new-directory-stack)
          (setf new-directory-stack nil)
          (incf current-depth))
        (values matches dir return-depth dir-stack-length)))))

(defun sort-matches (matches)
  "Sort matches by their correlation primarily string value secondarily"
  (stable-sort matches #'item<))

(defun item< (x y)
  (cond
    ((> (correlation y) (correlation x)) nil)
    ((< (correlation y) (correlation x)) t)
    ((= (correlation y) (correlation x))
     (if (string< (relative-path y) (relative-path x))
         t
       nil))))

(defun item-dir-path (item)
  "Return the full directory path to the item."
  (cl-fad:pathname-directory-pathname (path item)))



;; (defun walk-dirs-and-match (first-dir query-terms)
;;   (let ((matches (make-array 1 :fill-pointer 0 :adjustable t))
;;         (dirs (make-array 1 :fill-pointer 1 :adjustable t :initial-element first-dir))
;;         (new-dirs (make-array 1 :fill-pointer 0 :adjustable t))
;;         (items))

;;     ;; Look at the dir(s) in first-dir, try to match an item in that dir.
;;     ;; If none match, descend a layer deeper.
;;     (loop for dir across dirs
;;             (setf items (cl-fad:list-directory dir))
;;             (setf matches  (concatenate 'vector matches (collate-matching-items query-terms items)))

;;             (loop for item in items ; add directories to dirs
;;                do (when (cl-fad:directory-pathname-p item)
;;                     (vector-push-extend item new-dirs)))))
;;      (list matches new-dirs))

;; (defun get-the-matches (query-string &optional max-levels)
;;   (let ((query-terms (search-terms query-string))
;;         (first-dir (or (parse-dir-path query-string) (truename ".")))
;;         (max-levels (or max-levels 2))
;;         match-return-list matches)
;;     (loop for level from 0 to max-levels
;;          do (progn
;;               (setf match-return-list (walk-dirs-and-match first-dir query-terms))
;;               (setf matches (car match-return-list))
;;               (if  (> (length matches) 0)
;;                    (return  matches)
;;                  (setf dirs (cadr match-return-list)))))
;;     dirs))

;; (defun walk-n-dirs (top-level-dir &optional levels dirs)
;;   (let ((levels (or levels 9999)); crude infinite recursing...
;;         (items (make-array 1 :fill-pointer 0 :adjustable t))
;;         (dirs (or dirs (make-array 1 :fill-pointer 1 :adjustable t :initial-element top-level-dir)))
;;         (top-level-dir-name (namestring top-level-dir))
;;         (next-level-dirs (make-array 1 :fill-pointer 0 :adjustable t))
;;         (dir-item-list))

;;     (declare (vector items dirs next-level-dirs))
;;     (declare (list dir-item-list))

;;     (loop repeat (+ 1 levels)
;;        do (progn
;;             (loop for dir across dirs
;;                do (progn
;;                     (setf dir-item-list
;;                           (mapc (lambda (path)
;;                                   (let ((str (cl-ppcre:regex-replace
;;                                               (format nil "^~s" top-level-dir-name)
;;                                               (namestring dir)
;;                                               "")))
;;                                     (make-instance 'item
;;                                                    :path path
;;                                                    :relative-path str)))
;;                                 (cl-fad:list-directory dir)))
;;                     (loop for path in dir-item-list
;;                        do (progn
;;                             (vector-push-extend path items)
;;                             (when (cl-fad:directory-pathname-p path)
;;                               (vector-push-extend path next-level-dirs))))))
;;             (setf dirs next-level-dirs)
;;             (setf next-level-dirs (make-array 1 :fill-pointer 0 :adjustable t))
;;             (setf dir-item-list '())))
;;     items))
