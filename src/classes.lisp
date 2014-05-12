(in-package :sick-filing)

(defclass query ()
  ((search-string :initarg :search-string
                  :initform ""
                  :reader search-string)
   (changedp :initarg :changedp
             :initform nil
             :reader changedp)))

(defclass results ()
  ((results-list :initform '()
                 :accessor results-list)
   (index :accessor index)))

(defclass item ()
  ((path :initarg :path
         :accessor path
         :documentation "The absolute path of the item of type pathname")
   (relative-path :initarg :relative-path
                  :initform ""
                  :accessor relative-path
                  :documentation "The relative path of the item of type string")
   (correlation :initarg :correlation
                :initform 1
                :accessor correlation
                :documentation "How correlated the match is to the original query, low numbers = more correlated"))
  (:documentation "The object spawned for each item found in the filesystem"))

(defun strengthen-correlation (item &optional amount)
  (setf (correlation item) (* (+ 1 amount) (correlation item))))
