(defpackage #:sick-filing
  (:use :cl :asdf)
  (:export :main))

(in-package :sick-filing)

(defsystem sick-filing
  :name "sick-filing"
  :author "Siavash Sajjadi <sia.s.saj@gmail.com>"
  :description "Blow your tits off awesome filing tool (actually just an ido-mode rip off)"
  :depends-on (:cl-charms :cl-fad :cl-ppcre :cl-utilities :swank)
  :serial t
  :components
  ((:module "src"
            :components ((:file "classes")
                         (:file "helpers")
                         (:file "curses")
                         (:file "process-matches")
                         (:file "filing")))))
