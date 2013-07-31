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


(defun m-fun (n err)
  (if (zerop n)
      (if err
	(error "error")
	(backtrace-list))
      (let ((bt
	     (m-fun (1- n) err)))
	(format t "IN: ~a~%" n)
	bt)))

(defun test-stack-trace ()
  (let ((bt (m-fun 6 nil)))
    (%print-stack bt *standard-output*)
    bt))

(defun test-start ()
  (with-print-backtrace-on-error ()
    (m-fun 6 t)))


(defun test-frame ()
  (print-frame
   (nth 4 (test-stack-trace))))
