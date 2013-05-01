(defpackage #:clobber-demo
  (:use #:common-lisp))

(in-package #:clobber-demo)

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Transaction logging.

(defparameter *transaction-log* nil)

(defun execute (transaction-function &rest arguments)
  (apply transaction-function arguments)
  (clobber:log-transaction (cons transaction-function arguments)
			   *transaction-log*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Starting and stopping.

(defun start (filename)
  (setf *banks* '())
  (setf *transaction-log*
	(clobber:open-transaction-log
	 filename
	 (lambda (transaction)
	   (apply (car transaction) (cdr transaction))))))

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
    (execute 'add-customer jane b2)
    (execute 'add-customer bill b1)
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





