(in-package #:grg)

(define-file-handler "static")

(define-closing-handler (root) ()
  (with-html-output-to-string (*standard-output* nil :prologue t :indent t)
    (:html (:head (:title "GRG - Gratuitous Resource Gathering")
		  (:script :type "text/javascript" :src "/static/daimio_composite.js")
		  (:script :type "text/javascript" :src "/static/daimio_start.js"))
	   (:body 
	    (:script :data-daimio-template "resource" :language "daimio"
		     "{begin block | merge data __in}"
		     (:button :class "resource" :data-value "{_name}" "{_name}")
		     "{end block}")
	    (:script :data-daimio-template "technology" :language "daimio"
		     "{begin block | merge data __in}"
		     (:button :class "technology" :data-value "{_name}" "{_name}")
		     "{end block}")
	    (:div :id "resources")
	    (:ul
	     (:li "Balance: " (:span :id "balance-display"))
	     (:li "Skills: " (:span :id "skills-display"))
	     (:li "Income: " (:span :id "income-display"))
	     (:li "Built: " (:span :id "tech-display")))
	    (:div :id "technologies")))))

(defparameter *server* (bt:make-thread (lambda () (start 4242))))
