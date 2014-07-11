(in-package :sick-filing)

(defun string-cut-end (string &key keep cut)
  (cond
    (keep (subseq string 0 (- keep 1)))
    (cut (subseq string 0 (- (length string) cut)))))

(defun insert-into (type string sub pos)
  (let ((end (length string)))
    (concatenate type (subseq string 0 pos) sub (subseq string pos end))))

(defun concat-list (list &optional spacer)
  (format nil (concatenate 'string "~{~a~^" spacer "~}") list))
