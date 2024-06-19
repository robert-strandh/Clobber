(cl:in-package #:clobber-internal)

(defmethod open-transaction-log ((streams-pair cons) function)
  (let ((object-table (make-hash-table))
	(input-stream (car streams-pair))
	(output-stream (cdr streams-pair)))
    (unless (null input-stream) ; read the transaction log from the input stream
      (load-transaction-log input-stream function object-table))
    (make-transaction-log output-stream object-table)))

(defmethod open-transaction-log ((filename pathname) function)
  (let ((object-table (make-hash-table)))
    (load-transaction-log filename function object-table)
    (make-transaction-log filename object-table)))

;;; Internal protocol

(defmethod load-transaction-log ((input-stream stream) function object-table)
  (let ((*readtable* (copy-readtable)))
    (set-syntax *readtable* object-table)
    (loop for transaction = (read input-stream nil nil)
          until (null transaction)
          do (funcall function transaction))))

(defmethod load-transaction-log ((input string) function object-table)
  (with-input-from-string (input-stream input)
    (load-transaction-log input-stream function object-table)))

(defmethod load-transaction-log ((filename pathname) function object-table)
  (with-open-file (input-stream filename
				:direction :input
				:if-does-not-exist :create)
    (load-transaction-log input-stream function object-table)))

(defmethod make-transaction-log ((output-stream stream) object-table)
  (multiple-value-bind (object-id-table max-object-id)
      (make-object-id-table object-table)
    (make-instance 'transaction-log
                   :log-stream output-stream
                   :object-id-table object-id-table
                   :next-object-id (1+ max-object-id))))

(defmethod make-transaction-log ((output pathname) object-table)
  (let ((output-stream (open output
                             :direction :output
                             :if-exists :append)))
    (make-transaction-log output-stream object-table)))
