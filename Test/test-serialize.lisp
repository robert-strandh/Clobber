(cl:in-package #:clobber-test)

(defun test-serialize-cons ()
  (format *trace-output* "~&; serialize test~%")
  (assert (string=
	   (serialize-to-string (cons 'a 'b))
	   "#2!(CLOBBER-TEST::A . CLOBBER-TEST::B)")))

(defun test-serialize-generic-functions ()
  (defclass c1 ()
    ((%s1 :initform 1 :accessor a1)))
  (assert (string=
	   (serialize-to-string #'(setf a1))
	   "#2!(COMMON-LISP:SETF . #3!(CLOBBER-TEST::A1 . COMMON-LISP:NIL))")))

(defun test-serialize-hash-tables ()
  (let ((ht (make-hash-table :test #'equal))
	serialization-string
	reconstructed-ht)
    (setf (gethash 'key1 ht) :val1)
    (setf (gethash 'key2 ht) :val2)
    (setf serialization-string (serialize-to-string ht))
    #+(or)(print serialization-string)
    (with-input-from-string (in serialization-string)
      (with-transaction-log (log (cons in *standard-output*)
				 (lambda (just-read-transaction)
				   #+(or)(print just-read-transaction)
				   (setf reconstructed-ht
					 just-read-transaction)))
	(assert (hash-table-p reconstructed-ht))
	(assert (eql (hash-table-test reconstructed-ht)
		     (hash-table-test ht)))
	(assert (eql (hash-table-size reconstructed-ht)
		     (hash-table-size ht)))
	(assert (eql (hash-table-rehash-size reconstructed-ht)
		     (hash-table-rehash-size ht)))
	(assert (eql (hash-table-rehash-threshold reconstructed-ht)
		     (hash-table-rehash-threshold ht)))
	(maphash (lambda (key ht-val
		     &aux (reconstructed-ht-val (gethash key reconstructed-ht)))
		   (assert (eql reconstructed-ht-val ht-val)))
		 ht)))))

(defun test-serialize ()
  (test-serialize-cons)
  (test-serialize-generic-functions)
  (test-serialize-hash-tables))
