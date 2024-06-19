(cl:in-package #:clobber-test)

(defmacro with-string-transaction-log (symbol &body body)
  (let ((output-stream (gensym "OUTPUT-STREAM")))
    `(with-output-to-string (,output-stream)
       (let ((,symbol (make-transaction-log ,output-stream (make-hash-table))))
	 ,@body))))

(defun serialize-to-string (object)
  (with-string-transaction-log transaction-log    
    (serialize object transaction-log)))
