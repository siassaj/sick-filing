(asdf:defsystem :sick-filing
  :name "sick-filing"
  :version "0.1"
  :maintainer "Siavash Sajjadi"
  :author "Siavash Sajjadi <sia.s.saj@gmail.com>"
  :description "Blow your tits off awesome filing tool (actually just an ido-mode rip off)"
  :depends-on (:cl-charms)
  :serial t
  :components
  ((:module "src"
            :components ((:file "package")
                         (:file "curses")))))
