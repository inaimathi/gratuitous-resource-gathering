(in-package #:grg)

(define-file-handler "static")

(define-closing-handler (root) ()
  (with-html-output-to-string (*standard-output* nil :prologue t)
    (:html (:head (:title "GRG - Gratuitous Resource Gathering")
		  (:script :type "text/javascript" :src "/static/daimio_composite.js")
		  (:script :type "text/javascript" :src "/static/daimio_start.js"))
	   (:body 
	    (:button :class "resource" :data-value "wood" "Wood")
	    (:button :class "resource" :data-value "stone" "Stone")
	    (:button :class "resource" :data-value "food" "Food")
	    (:ul
	     (:li "Balance: " (:span :id "balance-display"))
	     (:li "Skills: " (:span :id "skills-display"))
	     (:li "Income: " (:span :id "income-display")))
	    (:button :class "building" :data-value "Lumber Yard" "Lumber Yard")
	    (:button :class "building" :data-value "Quarry" "Quarry")
	    (:button :class "building" :data-value "Farm" "Farm")))))

(define-json-handler (echo) (value)
  (publish! :the-game  value)
  :ok)

(define-stream-handler (yodel) ()
  (subscribe! :the-game sock))

(defparameter *server* (bt:make-thread (lambda () (start 4242))))
