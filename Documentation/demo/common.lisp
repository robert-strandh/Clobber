(defpackage #:clobber-demo/common
  (:use #:common-lisp)
  (:export
   #:bank
   #:accounts
   #:customers
	  
   #:account
   #:balance
   #:holder
	  
   #:person
   #:name
	  
   #:*banks*
   #:new-bank
   #:delete-bank
   #:add-customer
   #:add-account
   #:withdraw
   #:deposit
   #:transfer

   #:*transaction-log*
   #:stop))

(in-package #:clobber-demo/common)

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

(defun id (object)
  (mod (sxhash object) 100))

(defmethod print-object ((bank bank) stream)
  (prin1 `(:bank-id ,(id bank)
	   :accounts ,(accounts bank)
	    :customers ,(customers bank))
	  stream))

(defmethod print-object ((account account) stream)
  (prin1 `(:account-id ,(id account)
	    :parent-bank-id ,(id (bank account))
	    :balance ,(balance account)
	    :holder ,(holder account))
	  stream))

(defmethod print-object ((person person) stream)
  (format stream "<~A>" (name person)))
  
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
;;; Transaction logging by Clobber

(defparameter *transaction-log* nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Starting and stopping

(defun stop ()
  (when (clobber:transaction-log-open-p *transaction-log*)
    (clobber:close-transaction-log *transaction-log*)))

