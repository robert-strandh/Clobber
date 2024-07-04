(defpackage #:clobber-demo/demo3
  (:use #:common-lisp #:clobber-demo/common)
  (:import-from #:clobber-demo/demo2
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
		#:clear-uncommitted-to-list)
  (:export  #:with-atomic-logging

	    #:start
	    #:execute
	    
	    #:log-transaction
	    #:commit
	    #:clear-uncommitted
	    #:*atomic-logging*))

(in-package #:clobber-demo/demo3)

;;; Demonstration of multiple (execute ...) forms atomic logging 

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
	   (log-to-list transaction)
	   (commit-to-list)))))

(defun log-transaction (transaction)
  (clobber:log-transaction transaction        ; log to temporary buffer
			   *transaction-log*) 
  (log-to-list transaction))                  ; log to temporary list

(defun commit ()
  (clobber:commit *transaction-log*)          ; commit to file
  (commit-to-list))

(defun clear-uncommitted ()
  (clobber:clear-uncommitted *transaction-log*)
  (clear-uncommitted-to-list))

;; *atomic-logging* is true when we're inside
;; the body of the with-atomic-logging macro
(defvar *atomic-logging* nil)

;;; For convenience,
;;; when not inside the body of the with-atomic-logging macro
;;; the function #'execute calls (clear-uncommitted) and (commit)
;;; so that the execution of that single transaction is atomic
;;; without the need to wrap it with with-atomic-logging
;;; and without the need for the application programmer to manually
;;; call (clear-uncommitted) and (commit).

(defun execute (transaction-function &rest arguments)
  (let ((transaction (make-instance 'transaction
				    :function-name transaction-function
				    :arguments arguments)))
    ;; if we're inside a with-atomic-logging block no need to
    ;; (clear-uncommitted) because the macro with-atomic-logging
    ;; will handle it at the beginning of its block.
    (unless *atomic-logging* 
      (clear-uncommitted))
    ;; log to temporary buffer
    (log-transaction transaction)
    ;; execute
    (apply transaction-function arguments)
    ;; then commit if successful.
    ;; if we're inside a with-atomic-logging block no need to
    ;; (commit) because the macro with-atomic-logging
    ;; will handle it at the end of its block.
    (unless *atomic-logging*
      (commit))))

(defmacro with-atomic-logging (options &body body)
  ;; options is unused, but I added it because emacs indents
  ;; the form better if it sees the () in (with-atomic-logging () ...)
  ;; options could contain a specific database, as we'll see in demo4
  ;; which deals with an abstraction for databases so that an application
  ;; can use multiple databases at the same time, and also offers
  ;; a cleaner interface for application writers
  `(prog2 (clear-uncommitted)
       (let ((*atomic-logging* t))
	 ,@body)
     (commit)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The actual application
;;; now has an atomic block:

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
      (handler-case
	  (with-atomic-logging ()	; <- atomic block
	    ;; none of the withdrawals in this block will be logged,
	    ;; because an error happened before executing successfully
	    ;; the last transaction in the block
	    (execute 'withdraw 10 a3) ; <- this withdrawal is executed in the application
	    ;; and changes the objects, but it will not be logged.
	    ;; So after fixing the bug in the atomic logging block
	    ;; a simple reload-database command in the application can revert
	    ;; the state of all objects.
	    (execute 'withdraw (error "Some bug") a3)
	    (execute 'withdraw 10 a3)
	    (execute 'withdraw 10 a3))
	(error (c) "the buggy atomic block was ignored"))
      (execute 'transfer 20 a2 a1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Demonstrating the application
;;;
;;; (delete-database) ; clean up
;;; (do-and-see)      ; see what the database file contains after the execution of transactions
;;; (reload-database) ; see that *banks* has the data freshly revived from the database file. 


(defvar *database-file* (merge-pathnames "demo3-database" (user-homedir-pathname)))

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

