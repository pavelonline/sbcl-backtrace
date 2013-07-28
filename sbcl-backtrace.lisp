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
