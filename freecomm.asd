;;;; freecomm.asd

(asdf:defsystem #:freecomm
  :description "Describe freecomm here"
  :author "Anthony Le Cigne <dev@lecigne.net>"
  :license "Specify license here"
  :depends-on (#:cl-ppcre
               #:cl-csv)
  :serial t
  :components ((:file "package")
               (:file "freecomm")))

