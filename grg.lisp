(in-package #:grg)

(define-file-handler "static")

(define-closing-handler (root) ()
  (with-html-output-to-string (*standard-output* nil :prologue t :indent t)
    (:html (:head (:title "GRG - Gratuitous Resource Gathering")
		  (:link :rel "stylesheet" :href "/static/grg.css" :type "text/css" :media "screen")
		  (:script :type "text/javascript" :src "/static/daimio_composite.js")
		  (:script :type "text/javascript" :src "/static/daimio_start.js"))
	   (:body 
	    (:script :data-daimio-template "resource" :language "daimio"
		     "{begin block | merge data __in}"
		     (:button :class "resource" :data-value "{_name}" "{_name}")
		     "{end block}")
	    (:script :data-daimio-template "technology" :language "daimio"
		     "{begin block | merge data __in}"
		     (:div :class "tech-box"
			   (:ul (:li "Cost: {_cost}")
				(:li "Requires: {_requires}")
				(:li "Upgrade: {_upgrade}"))
			   (:button :class "technology" :data-value "{_name}" "{_name}"))
		     "{end block}")
	    (:div :id "resources")
	    (:ul
	     (:li "Balance: " (:span :id "balance-display"))
	     (:li "Storage: " (:span :id "storage-display"))
	     (:li "Skills: " (:span :id "skills-display"))
	     (:li "Income: " (:span :id "income-display"))
	     (:li "Built: " (:span :id "tech-display")))
	    (:div :id "technologies")))))

(defparameter *server* (bt:make-thread (lambda () (start 4242))))
