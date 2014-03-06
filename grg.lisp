(in-package #:grg)

(define-file-handler "static")

(define-closing-handler (root) ()
  (with-html-output-to-string (*standard-output* nil :prologue t)
    (:html (:head (:title "GRG - Gratuitous Resource Gathering")
		  (:script :type "text/javascript" :src "/static/daimio_composite.js")
		  (:script :type "text/javascript" :src "/static/daimio_start.js"))
	   (:body 
	    ;; (:script :type "text/daml")
	    (:form :id "go-form"
	     (:input :type "text" :name "color" :id "color")
	     (:input :type "text" :name "x" :id "x")
	     (:input :type "text" :name "y" :id "y")
	     (:input :type "submit"))
	    
	    (:script :data-daimio-template "piece" :language "daimio"
		     "{begin block | merge data __in}"
		     (:div :style "background-color: {_color}; left: {_x}px; top: {_y}px; width: 20px; height: 20px; border-radius: 10px; position:absolute;")
		     "{end block}")

	    (:div :id "board" :style "width: 200px; height: 200px; border: 1px solid #000;")	    
	    (:p :id "output" "Something!")))))

(define-json-handler (echo) (value)
  (publish! :the-game  value)
  :ok)

(define-stream-handler (yodel) ()
  (subscribe! :the-game sock))

(defparameter *server* (bt:make-thread (lambda () (start 4242))))








