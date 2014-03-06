;;;; daim-go.asd

(asdf:defsystem #:grg
  :serial t
  :description "Describe grg here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:house #:cl-who #:bordeaux-threads)
  :components ((:file "package")
               (:file "grg")))

