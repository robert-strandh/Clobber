(cl:in-package #:clobber-test)

(defun test-serialize ()
  (format *trace-output* "~&; serialize test~%")
  (assert (string=
	   (serialize-to-string (cons 'a 'b))
	   "#2!(CLOBBER-TEST::A . CLOBBER-TEST::B)")))
