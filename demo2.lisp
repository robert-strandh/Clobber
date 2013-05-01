(defpackage #:clobber-demo2
  (:use #:common-lisp))

(in-package #:clobber-demo2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The model.

(defclass bank ()
  ((%accounts :initform '() :accessor accounts)
   (%customers :initform '() :accessor customers)))

(defclass account ()
  ((%bank :initarg :bank :accessor bank)
   (%balance :initform 0 :accessor balance)
   (%holder :initarg :holder :reader holder)))

(clobber:define-save-info account
  (:holder holder))

(defclass person ()
  ((%name :initarg :name :reader name)))

(clobber:define-save-info person
  (:name name))

(defparameter *banks* '())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Transactions.

(defun new-bank (bank)
  (push bank *banks*))

(defun delete-bank (bank)
  (setf *banks* (remove bank *banks*)))

(defun add-customer (person bank)
  (push person (customers bank)))

(defun add-account (account bank)
  (push account (accounts bank))
  (setf (bank account) bank))

(defun withdraw (amount account)
  (decf (balance account) amount))

(defun deposit (amount account)
  (incf (balance account) amount))

(defun transfer (amount from-account to-account)
  (withdraw amount from-account)
  (deposit amount to-account))

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

;;; The transaction log.
(defparameter *transactions* '())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Transaction logging by Clobber

(defparameter *transaction-log* nil)

(defun execute (transaction-function &rest arguments)
  (apply transaction-function arguments)
  (let ((transaction (make-instance 'transaction
		       :function-name transaction-function
		       :arguments arguments)))
    (push transaction *transactions*)
    (clobber:log-transaction transaction
			     *transaction-log*)))

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
	   (push transaction *transactions*)
	   (apply (function-name transaction)
		  (arguments transaction))))))

(defun stop ()
  (clobber:close-transaction-log *transaction-log*))

(defun do-things-1 ()
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





