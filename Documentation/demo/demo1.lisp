(defpackage #:clobber-demo/demo1
  (:use #:common-lisp #:clobber-demo/common)
  (:export #:do-things))

(in-package #:clobber-demo/demo1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Transaction logging by Clobber

(defun execute (transaction-function &rest arguments)
  (clobber:clear-uncommitted *transaction-log*)
  (clobber:log-transaction (cons transaction-function arguments)
			   *transaction-log*)
  (apply transaction-function arguments)
  (clobber:commit *transaction-log*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Starting and stopping

(defun start (filename)
  (setf *banks* '())
  (setf *transaction-log*
	(clobber:open-transaction-log
	 filename
	 (lambda (transaction)
	   (apply (car transaction) (cdr transaction))))))

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
    (execute 'add-customer jane b2)
    (execute 'add-customer bill b1)
    (handler-case
	(execute 'add-customer typo1 typo2) ; error happens so it will not be committed
      (error ()))
    (let ((a1 (make-instance 'account :holder jane))
	  (a2 (make-instance 'account :holder jane))
	  (a3 (make-instance 'account :holder bill)))
      (execute 'add-account a1 b1)
      (execute 'add-account a2 b2)
      (execute 'add-account a3 b1)
      (execute 'deposit 100 a1)
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


(defvar *database-file* (merge-pathnames "demo1-database" (user-homedir-pathname)))

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

