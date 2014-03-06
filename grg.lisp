(in-package #:grg)

(define-file-handler "static")

(define-closing-handler (root) ()
  (with-html-output-to-string (*standard-output* nil :prologue t)
    (:html (:head (:title "GRG - Gratuitous Resource Gathering")
		  (:script :type "text/javascript" :src "/static/daimio_composite.js")
		  (:script :type "text/javascript" :src "/static/daimio_start.js"))
	   (:body 
	    (:button :id "resource" "Resource")
	    (:div :id "display" :style "width: 200px; height: 50px; border: 1px dashed #000;")
	    (:button :id "building" "Building")))))

(define-closing-handler ())

(define-json-handler (echo) (value)
  (publish! :the-game  value)
  :ok)

(define-stream-handler (yodel) ()
  (subscribe! :the-game sock))

(defparameter *server* (bt:make-thread (lambda () (start 4242))))
