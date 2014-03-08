(in-package #:grg)

(define-file-handler "static")

(define-closing-handler (root) ()
  (with-html-output-to-string (*standard-output* nil :prologue t)
    (:html (:head (:title "GRG - Gratuitous Resource Gathering")
		  (:script :type "text/javascript" :src "/static/daimio_composite.js")
		  (:script :type "text/javascript" :src "/static/daimio_start.js"))
	   (:body 
	    (:button :id "lumberjack" "Wood")
	    (:button :id "quarry" "Stone")
	    (:button :id "forage" "Food")
	    (:div :id "balance-display" :style "width: 200px; height: 50px; border: 1px dashed #000;")
	    (:button :class "building" "Lumber Yard")
	    (:button :class "building" "Quarry")
	    (:button :class "building" "Farm")))))

(define-json-handler (echo) (value)
  (publish! :the-game  value)
  :ok)

(define-stream-handler (yodel) ()
  (subscribe! :the-game sock))

(defparameter *server* (bt:make-thread (lambda () (start 4242))))
