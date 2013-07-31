(in-package :asdf)

(defsystem "sbcl-backtrace"
  :description "hello-lisp: a sample Lisp system."
  :version "0.0.1"
  :author "Pavel Kukushkin <k.pavel.g@gmail.com>"
  :licence "Public Domain"
  :components ((:file "package")
	       (:file "swank-parser-import" :depends-on ("package"))
	       (:file "slime-imports" :depends-on ("package" "swank-parser-import"))
	       (:file "sbcl-backtrace" :depends-on ("package" "slime-imports"))
	       (:file "test" :depends-on ("package" "sbcl-backtrace"))))
		     
