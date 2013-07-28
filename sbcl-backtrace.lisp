(in-package :sbcl-backtrace)


(define-condition information-retrieve-error (error)
  ((text :initarg :text
	 :reader text)
   (object :initarg :object
	   :reader object)))

(defun backtrace-list ()
  "Return stack of the call as list starting from top call"
  (loop for frame = (sb-di:top-frame)
     then (sb-di:frame-down frame)
     while frame
     collect frame))

(defmacro with-print-backtrace-on-error ((&optional (stream '*standard-output*)) &body body)
  (let ((stream-var (gensym "STREAM")))
    `(let ((,stream-var ,stream))
       (handler-bind ((error (lambda (c)
			       (print-stack ,stream-var)
			       (error c))))
	 ,@body))))

(defun frame-debug-values (frame)
  (let ((vars (frame-debug-vars frame)))
    (when vars
      (loop for var across vars
	 collect
	   (list
	    (sb-di:debug-var-symbol var)
	    (handler-case
		(sb-di:debug-var-valid-value var frame)
	      (sb-di:invalid-value ()
		0)))))))
    
(defun vec-to-list (vec)
  (when vec
    (loop for elt across vec collect elt)))

(defun print-frame (frame &optional (stream *standard-output*))
  ;; FORMAT <stack-number>:<file-name>:<position>:<functionname>args
  (with-accessors ((debug-fn sb-di:frame-debug-fun)
		   (number sb-di:frame-number)
		   (code-location sb-di:frame-code-location))
      frame
    (let ((location (handler-case
			(code-location-source-location-excp code-location)
		      (information-retrieve-error () nil)))
	  (values (frame-debug-values frame)))
      (format stream
	      "~a:~:[<undefined>~;~:*~a~]:~:[<undefined>~;~:*~a~]:~a~[~:;~%ARGS: (~a)~]~%"
	      number
	      (location-buffer location)
	      (location-position location)
	      (sb-di:debug-fun-name debug-fn)
	      (length values)
	      (length values)))))

(defun print-stack (&optional (stream *standard-output*))
  (dolist (frame (backtrace-list))
    (print-frame frame stream)))
  
  
