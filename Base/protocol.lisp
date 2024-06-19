(cl:in-package #:clobber-internal)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Transaction log.
;;;
;;; Transactions can be any object.  They are logged to the stream by
;;; using the serialization defined below.  Typically, an application
;;; would either use a list (<function-name> <arg> ... <arg>) or an
;;; instance of STANDARD-OBJECT as a transaction.  The application
;;; itself is responsible for what to do with a transaction when a
;;; transaction log is opened.

(defclass transaction-log ()
  (;; A stream to which transactions are logged.
   (%log-streami
    :initarg :log-stream
    :reader log-stream)
   ;; A table mapping objects to identities.  Whenever we serialize an
   ;; object that has not been serialized before, it is entered into
   ;; the table, and the output is marked with the identity.  Whenever
   ;; we serialize an object that has been serialized before, we just
   ;; print the identity.
   (%object-id-table
    :initarg :object-id-table
    :reader object-id-table)
   ;; We allocate object identities sequentially.
   (%next-object-id
    :initarg :next-object-id
    :accessor next-object-id)))

;;; Serialization of instances of STANDARD-OBJECT.

(defmacro define-save-info (type &body save-info)
  `(defmethod save-info append ((obj ,type))
     ',save-info))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Open a transaction log.
;;;
;;; This is a compound function that first calls LOAD-TRANSACTION-LOG
;;; to load a transaction log from an input stream and execute the provided
;;; function on its contents, then calls MAKE-TRANSACTION-LOG which
;;; utilizes the object-table modified in effect of the first function
;;; to create a transaction log object which can be used to log further
;;; transactions.
;;; INPUT-AND-OUTPUT can be a pair of streams or a single pathname

(defgeneric open-transaction-log (input-and-output function))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Close a transaction log.

(defun close-transaction-log (transaction-log)
  (close (log-stream transaction-log)))

(defun transaction-log-open-p (transaction-log)
  (and
   (not (null transaction-log))
   (open-stream-p (log-stream transaction-log))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Log a transaction.
;;;
;;; A transaction can be any object that the application sees fit.

(defun log-transaction (transaction transaction-log)
  (serialize transaction transaction-log)
  (terpri (log-stream transaction-log))
  (finish-output (log-stream transaction-log)))

(defmacro with-transaction-log ((var file-or-streams-pair function) &body forms)
  `(let ((,var (open-transaction-log ,file-or-streams-pair ,function)))
     (unwind-protect (progn ,@forms)
       (close-transaction-log ,var))))

;;; Internal protocol


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Load a transaction log.
;;;
;;; This function reads the transaction log from the input stream.
;;;
;;; After a transaction has been read, the function supplied by the
;;; application is called in order to execute the transaction.

(defgeneric load-transaction-log (input function object-table))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Make the transaction log object.
;;;
;;; This function takes an output which can be a stream or a pathname
;;; and an object table, creates an object identity table
;;; based on its context, and uses the ID table to create a
;;; transaction-log object that will be used to log new transactions.
	
(defgeneric make-transaction-log (output object-table))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Serialization.
;;;
;;; What we are doing is essentially a duplication of what
;;; print-object is doing, because unfortunately, there is no portable
;;; way of using print-object to accomplish what we want.

(defgeneric serialize (object transaction-log))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Serialization of instances of STANDARD-OBJECT.

(defgeneric save-info (object)
  (:method-combination append :most-specific-last))

(defmethod save-info append ((object standard-object))
  '())
