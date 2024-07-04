(defpackage #:clobber-demo/demo2
  (:use #:common-lisp #:clobber-demo/common)
  (:export #:do-things
	   #:*operator*
	   #:*comment*
	   #:with-comment
	   #:transaction
	   #:function-name
	   #:arguments
	   #:creator
	   #:comment
	   #:*transactions*
	   #:log-to-list
	   #:commit-to-list
	   #:clear-uncommitted-to-list))

(in-package #:clobber-demo/demo2)

;;; In this demo we keep a "transaction log" which is a 1-to-1 mapping
;;; of the Clobber transaction log, but which can be used to query the
;;; system for information such as "who closed that account?"  "when
;;; was it done?", etc.

;;; The person operating the system.
(defparameter *operator* "Suzy")

;;; The comment to store in the current transaction.
(defparameter *comment* "")

;;; Wrap the execution of a a transaction in this macro
;;; if a comment is desired. 
(defmacro with-comment (comment &body body)
  `(let ((*comment* ,comment))
     ,@body))

(defclass transaction ()
  ((%function-name :initarg :function-name :reader function-name)
   (%arguments :initarg :arguments :reader arguments)
   (%creator :initform *operator* :initarg :creator :reader creator)
   (%creation-date :initform (get-universal-time)
		   :initarg :creation-date
		   :reader creation-date)
   (%comment :initform *comment* :initarg :comment :reader comment)))

(clobber:define-save-info transaction
  (:function-name function-name)
  (:arguments arguments)
  (:creator creator)
  (:creation-date creation-date)
  (:comment comment))

(defmethod print-object ((tr transaction) stream)
  (progn ;;print-unreadable-object (tr stream :type nil :identity nil)
    (with-accessors ((function-name function-name)
		     (arguments arguments)) tr
      (format stream "#T(~A ~{~A~^ ~})" function-name arguments))))

;;; The transaction log mirrored as a list
(defparameter *transactions* '())
(defparameter *tmp-transactions* '())

(defun log-to-list (transaction)
  (push transaction *tmp-transactions*))

(defun commit-to-list ()
  (setf *transactions* (append *tmp-transactions* *transactions*))
  (setf *tmp-transactions* '()))

(defun clear-uncommitted-to-list ()
  (setf *tmp-transactions* '()))

(defun execute (transaction-function &rest arguments)
  ;; if an error happened during a previous execution
  ;; of some transaction, 
  ;; remove what was logged temporarily but not committed
  (clobber:clear-uncommitted *transaction-log*)
  (clear-uncommitted-to-list)
  
  (let ((transaction (make-instance 'transaction
				    :function-name transaction-function
				    :arguments arguments)))
    ;; log the transaction to a temporary buffer
    ;; before executing it, because the execution
    ;; may change the objects from the transaction
    ;; which is to be logged
    ;; (we want to log the arguments for the function
    ;;  as they were before executing the function)
    (clobber:log-transaction transaction
			     *transaction-log*)
    ;; also log to a temporary list that will be committed
    ;; to our list that mirrors the log file
    (log-to-list transaction) 
    ;; now execute the transaction
    (apply transaction-function arguments)
    ;; if the execution was successful,
    ;; commit the temporary buffer to the file
    (clobber:commit *transaction-log*)
    ;; also commit to our list that mirrors the log file
    (commit-to-list)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Starting and stopping.

(defun start (filename)
  (setf *banks* '())
  (setf *transactions* '())
  (setf *transaction-log*
	(clobber:open-transaction-log
	 filename
	 (lambda (transaction)
	   (apply (function-name transaction)
		  (arguments transaction))
	   (push transaction *transactions*)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The actual application

(defun do-things ()
  (let ((b1 (make-instance 'bank))
	(b2 (make-instance 'bank))
	(jane (make-instance 'person :name "Jane"))
	(bill (make-instance 'person :name "Bill")))
    (execute 'new-bank b1)
    (execute 'new-bank b2)
    (execute 'add-customer jane b1)
    (with-comment "What does bank 2 have that bank 1 does not?"
      (execute 'add-customer jane b2))
    (execute 'add-customer bill b1)
    (let ((a1 (make-instance 'account :holder jane))
	  (a2 (make-instance 'account :holder jane))
	  (a3 (make-instance 'account :holder bill)))
      (execute 'add-account a1 b1)
      (execute 'add-account a2 b2)
      (execute 'add-account a3 b1)
      (with-comment "Gee, I wish they would deposit more money!"
	(execute 'deposit 100 a1))
      (execute 'deposit 200 a2)
      (execute 'deposit 300 a3)
      (execute 'withdraw 10 a3)
      (execute 'transfer 20 a2 a1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Demonstrating the application
;;;
;;; (delete-database) ; clean up
;;; (do-and-see)      ; see what the database file contains after the execution of transactions
;;; (reload-database) ; see that *banks* has the data freshly revived from the database file. 


(defvar *database-file* (merge-pathnames "demo2-database" (user-homedir-pathname)))

(defun do-and-see ()
  (start *database-file*)
  (do-things)
  ;;; inspect the file to see the transaction log
  (with-open-file (stream *database-file*)
    (let ((data (make-string (file-length stream))))
      (read-sequence data stream)
      data)))

(defun reload-database ()
  (stop)
  (start *database-file*))

(defun reload-database-and-see ()
  (reload-database)
  *banks*)
  
(defun delete-database ()
  (stop)
  (when (probe-file *database-file*)
    (delete-file *database-file*)))
