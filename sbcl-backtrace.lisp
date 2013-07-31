(in-package :sbcl-backtrace)


(define-condition information-retrieve-error (error)
  ((text :initarg :text
	 :reader text)
   (object :initarg :object
	   :reader object)))

(defgeneric pretty-print (object stream)
  (:documentation "Pretty print object"))

(defclass pretty-printable () ())

(defmethod pretty-print ((obj (eql nil)) stream)
  (format stream "NIL"))

(defgeneric to-string (obj)
  (:documentation "Pretty string representation of obj"))

(defmethod to-string ((obj (eql nil)))
  (values "NIL"))

(defmethod to-string ((obj pretty-printable))
  (with-output-to-string (s)
    (pretty-print obj s)))
      

(defmethod print-object ((object pretty-printable) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (pretty-print object stream)))

(defclass stack-frame (pretty-printable)
  ((number :initarg :number :reader stack-number
	     :initform (error "Mandatory field"))
   (name :initarg :name :reader name
	 :initform (error "Mandatory field"))
   (location :initarg :location :reader location
	     :initform (error "Mandatory field"))
   (args :initarg :args :reader args
	 :initform (error "Mandatory field"))))

(defmethod pretty-print ((object stack-frame) stream)
  ;; FORMAT <stack-number>:<file-name>:<position>:<functionname>args
  (format stream "~a:~a:~a(~{~a~^, ~})"
	  (stack-number object)
	  (to-string (location object))
	  (name object)
	  (args object)))


(defun frame-location (frame)
  (with-accessors ((debug-fn sb-di:frame-debug-fun)
		   (number sb-di:frame-number)
		   (code-location sb-di:frame-code-location))
      frame
    (handler-case
	(code-location-source-location-excp code-location)
      (information-retrieve-error () nil))))

(defun frame-fun-name (frame)
  (sb-di:debug-fun-name (sb-di:frame-debug-fun frame)))

(defun frame-args (frame)
  (let ((code-location (sb-di:frame-code-location frame))
	(all-vars (sb-di::debug-fun-debug-vars (sb-di:frame-debug-fun frame))))
    (mapcar (lambda (var)
	      (sb-di:debug-var-value var frame))
	    (vec-to-list
	     (remove-if (lambda (var)
			  (ecase (sb-di:debug-var-validity var code-location)
			    (:valid nil)
			    ((:invalid :unknown) t)))
			all-vars)))))

(defun %frame-to-stack-frame (frame)
  (make-instance 'stack-frame
		 :number
		 (sb-di:frame-number frame)
		 :location
		 (frame-location frame)
		 :args
		 (frame-args frame)
		 :name (frame-fun-name frame)))

(defun %stack-list ()
  (loop for frame = (sb-di:top-frame)
     then (sb-di:frame-down frame)
     while frame
     collect (%frame-to-stack-frame frame)))

  

(defun backtrace-list ()
  "Return stack of the call as list starting from top call"
  (%stack-list))

(defun vec-to-list (vec)
  (when vec
    (loop for elt across vec collect elt)))

(defun %print-stack (bt stream)
  (dolist (frame bt)
    (pretty-print frame stream)
    (format stream "~%")))

(defun print-stack (&optional (stream *standard-output*))
  (%print-stack (backtrace-list) stream))
  
(defmacro with-print-backtrace-on-error ((&optional (stream '*standard-output*)) &body body)
  "Print stack on any error and then raise caught conition"
  (let ((stream-var (gensym "STREAM")))
    `(let ((,stream-var ,stream))
       (handler-bind ((error (lambda (c)
			       (print-stack ,stream-var)
			       (error c))))
	 ,@body))))

