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


(defun m-fun (n)
  (if (zerop n)
      (error "error")
      (let ((bt
	     (m-fun (1- n))))
	(format t "IN: ~a~%" n)
	bt)))

(defun test-stack-trace ()
  (m-fun 6))

(defun test-start ()
  (with-print-backtrace-on-error ()
    (m-fun 6)))


(defun test-frame ()
  (print-frame
   (nth 4 (test-stack-trace))))
