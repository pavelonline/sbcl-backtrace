(in-package :sbcl-backtrace)

;;;;;

(define-condition error-raised-condition (condition)
  ((original-condition
    :initarg :original-condition
    :accessor original-condition))
  (:report (lambda (condition stream)
             (format stream "Condition in debugger code~@[: ~A~]" 
                     (original-condition condition))))
  (:documentation
   "Wrapper for conditions that should not be debugged.

When a condition arises from the internals of the debugger, it is not
desirable to debug it -- we'd risk entering an endless loop trying to
debug the debugger! Instead, such conditions can be reported to the
user without (re)entering the debugger by wrapping them as
`sldb-condition's."))

;;;;;

(defun raise-find-location-error (code-location)
  (error 'information-retrieve-error
	 :text "Cannot find location for ~a"
	 :object code-location))

;;;;;


(defstruct (:location (:type list) :named
                      (:constructor make-location
                                    (buffer position &optional hints)))
  buffer position
  ;; Hints is a property list optionally containing:
  ;;   :snippet SOURCE-TEXT
  ;;     This is a snippet of the actual source text at the start of
  ;;     the definition, which could be used in a text search.
  hints)


;;;;;

(defmacro with-definition-source ((&rest names) obj &body body)
  "Like with-slots but works only for structs."
  (flet ((reader (slot)
           ;; Use read-from-string instead of intern so that
           ;; conc-name can be a string such as ext:struct- and not
           ;; cause errors and not force interning ext::struct-
           (read-from-string
            (concatenate 'string "sb-introspect:definition-source-" 
                         (string slot)))))
    (let ((tmp (gensym "OO-")))
      ` (let ((,tmp ,obj))
          (symbol-macrolet
              ,(loop for name in names collect 
                     (typecase name
                       (symbol `(,name (,(reader name) ,tmp)))
                       (cons `(,(first name) (,(reader (second name)) ,tmp)))
                       (t (error "Malformed syntax in WITH-STRUCT: ~A" name))))
            ,@body)))))

(defun categorize-definition-source (definition-source)
  (with-definition-source (pathname form-path character-offset plist)
    definition-source
    (let ((file-p (and pathname (probe-file pathname)
                       (or form-path character-offset))))
      (cond (file-p :file)
            (pathname :file-without-position)
            (t :invalid)))))

(defun definition-source-file-location (definition-source)
  (with-definition-source (pathname form-path character-offset plist)
      definition-source
    (let* ((namestring (namestring (translate-logical-pathname pathname)))
           (pos (if form-path
                    (source-file-position namestring form-path)
                    character-offset)))
      (make-location namestring
                     ;; /file positions/ in Common Lisp start from
                     ;; 0, buffer positions in Emacs start from 1.
                     (1+ pos)))))

(defun definition-source-for-emacs (definition-source type name)
  (with-definition-source (pathname)
      definition-source
    (ecase (categorize-definition-source definition-source)
      (:file
       (definition-source-file-location definition-source))
      (:file-without-position
       (make-location (namestring 
		       (translate-logical-pathname pathname))
                      1))
      (:invalid
       (error "DEFINITION-SOURCE of ~(~A~) ~A did not contain ~
               meaningful information."
              type name)))))

(defun source-file-position (filename form-path)
  (with-open-file (s filename)
    (source-path-stream-position form-path s)))

(defun function-source-location (function &optional name)
  (declare (type function function))
  (definition-source-for-emacs (sb-introspect:find-definition-source function)
                               :function
                               (or name (sb-impl::%fun-name function))))

;;;;;

;;;; Code-location -> source-location translation

;;; If debug-block info is avaibale, we determine the file position of
;;; the source-path for a code-location.  If the code was compiled
;;; with C-c C-c, we have to search the position in the source string.
;;; If there's no debug-block info, we return the (less precise)
;;; source-location of the corresponding function.

(defun code-location-source-location (code-location)
  (let ((dsource (sb-di:code-location-debug-source code-location)))
    (if (sb-di:debug-source-namestring dsource)
	(file-source-location code-location)
	(raise-find-location-error code-location))))

;;; FIXME: The naming policy of source-location functions is a bit
;;; fuzzy: we have FUNCTION-SOURCE-LOCATION which returns the
;;; source-location for a function, and we also have FILE-SOURCE-LOCATION &co
;;; which returns the source location for a _code-location_.
;;;
;;; Maybe these should be named code-location-file-source-location,
;;; etc, turned into generic functions, or something. In the very
;;; least the names should indicate the main entry point vs. helper
;;; status.

(defun file-source-location (code-location)
  (if (code-location-has-debug-block-info-p code-location)
      (source-file-source-location code-location)
      (fallback-source-location code-location)))

(defun fallback-source-location (code-location)
  (let ((fun (code-location-debug-fun-fun code-location)))
    (cond (fun (function-source-location fun))
          (t (error "Cannot find source location for: ~A " code-location)))))

(defun read-file (filename)
  "Return the entire contents of FILENAME as a string."
  (with-open-file (s filename :direction :input)
    (let* ((string (make-string (file-length s)))
           (length (read-sequence string s)))
      (subseq string 0 length))))

(defun source-file-source-location (code-location)
  (let* ((filename (code-location-debug-source-name code-location)))
    (with-open-file (s filename)
      (let* ((pos (stream-source-position code-location s)))
	(make-location filename
		       pos)))))

(defun code-location-debug-source-name (code-location)
  (namestring (truename (#+#.(swank-backend:with-symbol
                              'debug-source-name 'sb-di)
                             sb-c::debug-source-name
                             #-#.(swank-backend:with-symbol
                                  'debug-source-name 'sb-di)
                             sb-c::debug-source-namestring
                         (sb-di::code-location-debug-source code-location)))))

(defun code-location-debug-source-created (code-location)
  (sb-c::debug-source-created
   (sb-di::code-location-debug-source code-location)))

(defun code-location-debug-fun-fun (code-location)
  (sb-di:debug-fun-fun (sb-di:code-location-debug-fun code-location)))

(defun code-location-has-debug-block-info-p (code-location)
  (handler-case
      (progn (sb-di:code-location-debug-block code-location)
             t)
    (sb-di:no-debug-blocks  () nil)))

(defun stream-source-position (code-location stream)
  (let* ((cloc (sb-debug::maybe-block-start-location code-location))
	 (tlf-number (sb-di::code-location-toplevel-form-offset cloc))
	 (form-number (sb-di::code-location-form-number cloc)))
    (multiple-value-bind (tlf pos-map) (read-source-form tlf-number stream)
      (let* ((path-table (sb-di::form-number-translations tlf 0))
             (path (cond ((<= (length path-table) form-number)
                          (warn "inconsistent form-number-translations")
                          (list 0))
                         (t
                          (reverse (cdr (aref path-table form-number)))))))
        (source-path-source-position path tlf pos-map)))))

(defun string-source-position (code-location string)
  (with-input-from-string (s string)
    (stream-source-position code-location s)))

(defun frame-debug-vars (frame)
  "Return a vector of debug-variables in frame."
  (sb-di::debug-fun-debug-vars (sb-di:frame-debug-fun frame)))
