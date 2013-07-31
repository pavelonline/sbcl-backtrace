(in-package :sbcl-backtrace)

(defun code-location-source-location-excp (code-location)
  (handler-case
	(code-location-source-location code-location)
    (error ()
      (error 'information-retrieve-error
	     :text "Cannot find source location for ~a"
	     :object code-location))))

(defmethod print-object ((object information-retrieve-error) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream (text object) (object object))))

(defun location-str (location)
  (if location
      (format nil "~a:~a" (location-buffer location)
	      (location-position location))
      "<location-unavailable>"))

(defun call-in-stack-depth (n fun)
  (if (zerop n)
      (funcall fun)
      (progn
      (let ((res (call-in-stack-depth (1- n) fun)))
	res))))

(defun test-stack-trace ()
  (call-in-stack-depth 6
		       #'print-stack))
		       

(defun test-frame ()
  (print-frame
   (nth 4 (test-stack-trace))))

(defun show-frame-args (frame#)
  (let* ((frame (nth frame# (backtrace-list)))
	 (code-location (sb-di:frame-code-location frame))
	 (all-vars (sb-di::debug-fun-debug-vars (sb-di:frame-debug-fun frame))))
    (list
     frame
     all-vars
     (mapcar (lambda (var)
	       (sb-di:debug-var-value var frame))
	     (vec-to-list
	      (remove-if (lambda (var)
			   (ecase (sb-di:debug-var-validity var code-location)
			     (:valid nil)
			     ((:invalid :unknown) t)))
			all-vars))))))
  
