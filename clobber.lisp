(cl:in-package #:clobber)

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
;;; Serialization of objects other than instances of STANDARD-OBJECT.

;;; A number is serialized as it is normally printed.

(defmethod serialize ((object number) transaction-log)
  (prin1 object (log-stream transaction-log)))

;;; A character is serialized as it is normally printed.

(defmethod serialize ((object character) transaction-log)
  (prin1 object (log-stream transaction-log)))

;;; A symbol is serialized by printing it with its package prefix.  We
;;; accomplish this by setting *package* to the keyword package while
;;; printing the symbol.

(defmethod serialize ((object symbol) transaction-log)
  (let ((*package* (find-package '#:keyword)))
    (prin1 object (log-stream transaction-log))))

(defmethod serialize ((object cons) transaction-log)
  (with-accessors ((object-id-table object-id-table)
                   (next-object-id next-object-id)
                   (log-stream log-stream))
      transaction-log
    (let ((id (gethash object object-id-table)))
      (if (null id)
          (progn
            (setf id (incf next-object-id))
            (setf (gethash object object-id-table) id)
            (format log-stream "#~d!(" id)
            (serialize (car object) transaction-log)
            (format log-stream " . ")
            (serialize (cdr object) transaction-log)
            (format log-stream ")"))
          (format log-stream "#~d^" id)))))

;;; FIXME: add serialization of arbitrary arrays.

(defmethod serialize ((object vector) transaction-log)
  (with-accessors ((object-id-table object-id-table)
                   (next-object-id next-object-id)
                   (log-stream log-stream))
      transaction-log
    (let ((id (gethash object object-id-table)))
      (if (null id)
          (progn (setf id (incf next-object-id))
                 (setf (gethash object object-id-table) id)
                 (format log-stream "#~d!#(" id)
                 (loop for element across object
                       do (serialize element transaction-log)
                          (format log-stream " "))
                 (format log-stream ")"))
          (format log-stream "#~d^" id)))))

(defmethod serialize ((object string) transaction-log)
  (with-accessors ((object-id-table object-id-table)
                   (next-object-id next-object-id)
                   (log-stream log-stream))
      transaction-log
    (let ((id (gethash object object-id-table)))
      (if (null id)
          (progn (setf id (incf next-object-id))
                 (setf (gethash object object-id-table) id)
                 (format log-stream "#~d!\"" id)
                 (loop for char across object
                       do (cond ((eql char #\")
                                 (format log-stream "\\\""))
                                ((eql char #\\)
                                 (format log-stream "\\\\"))
                                (t
                                 (princ char log-stream))))
                 (format log-stream "\""))
          (format log-stream "#~d^" id)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Serialization of instances of STANDARD-OBJECT.

(defgeneric save-info (object)
  (:method-combination append :most-specific-last))

(defmethod save-info append ((object standard-object))
  '())

(defmacro define-save-info (type &body save-info)
  `(defmethod save-info append ((obj ,type))
     ',save-info))

(defmethod serialize ((object standard-object) transaction-log)
  (with-accessors ((object-id-table object-id-table)
                   (next-object-id next-object-id)
                   (log-stream log-stream))
      transaction-log
    (let ((id (gethash object object-id-table)))
      (if (null id)
          (progn (setf id (incf next-object-id))
                 (setf (gethash object object-id-table) id)
                 (format log-stream "#~d![" id)
                 (serialize (class-name (class-of object)) transaction-log)
                 (loop for info in (save-info object)
                       do (format log-stream " ")
                          (serialize (car info) transaction-log)
                          (format log-stream " ")
                          (serialize (funcall (cadr info) object)
                                     transaction-log))
                 (format log-stream "]"))
          (format log-stream "#~d^" id)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Open a transaction log.
;;;
;;; This is a compound function that first calls LOAD-TRANSACTION-LOG
;;; to load a transaction log from disk and execute the provided
;;; function on its contents, then utilizes the object-table modified
;;; in effect of the first function to create a transaction log object
;;; which can be used to log further transactions.

(defun open-transaction-log (filename function)
  "Load transaction log from FILENAME and return an instance of `clobber:transaction-log'.

FUNCTION should accept a single argument, a transaction object (see
`clobber:load-transaction-log')."
  (let ((object-table (make-hash-table)))
    (load-transaction-log filename function object-table)
    (make-transaction-log filename object-table)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Load a transaction log.
;;;
;;; This function reads the transaction log from disk.
;;;
;;; After a transaction has been read, the function supplied by the
;;; application is called in order to execute the transaction.

(defun load-transaction-log (filename function object-table)
  "Read transaction log from FILENAME, calling FUNCTION for each transaction.

FUNCTION should accept a single argument, a transaction object.

OBJECT-TABLE should be a hash table."
  (with-open-file (stream filename
                          :direction :input
                          :if-does-not-exist :create)
    (let ((*readtable* (copy-readtable)))
      (set-syntax *readtable* object-table)
      (loop for transaction = (read stream nil nil)
            until (null transaction)
            do (funcall function transaction)))))

;;; This function returns two values: the object identity table which
;;; is required for creating the transaction log object and the
;;; maximum object ID present in the table. Integers above this
;;; number are free for assignment in subsequent transactions.

(defun make-object-id-table (object-table)
  (let* ((object-id-table (make-hash-table :test #'eq))
         (max-object-id 0)
         (fn (lambda (object-id object)
               (setf (gethash object object-id-table) object-id
                     max-object-id (max max-object-id object-id)))))
    (maphash fn object-table)
    (values object-id-table max-object-id)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Make the transaction log object.
;;;
;;; This function takes an object table, creates an object identity
;;; table based on its context, and uses the ID table to create a
;;; transaction-log object that will be used to log new transactions.

(defun make-transaction-log (filename object-table)
  (multiple-value-bind (object-id-table max-object-id)
      (make-object-id-table object-table)
    (let ((stream (open filename
                        :direction :output
                        :if-exists :append)))
      (make-instance 'transaction-log
                     :log-stream stream
                     :object-id-table object-id-table
                     :next-object-id (1+ max-object-id)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Syntax-setting functions for the transaction log loader.
;;;
;;; The readtable used to read a transaction contains three additional
;;; reader macros, associated with #!, #^ and [.

(defun set-syntax (readtable object-table)
  (let ((*readtable* readtable))
    (%set-syntax-left-bracket )
    (%set-syntax-right-bracket)
    (%set-syntax-hash-bang object-table)
    (%set-syntax-hash-caret object-table)))

(defun %set-syntax-left-bracket ()
  (set-macro-character
   #\[
   (lambda (stream char)
     (declare (ignore char))
     (apply #'make-instance (read-delimited-list #\] stream t)))))

(defun %set-syntax-right-bracket ()
  (set-syntax-from-char #\] #\)))

(defun %set-syntax-hash-bang (object-table)
  (set-dispatch-macro-character
   #\# #\!
   (lambda (stream char param)
     (declare (ignore char))
     (setf (gethash param object-table)
           (read stream nil nil t)))))

(defun %set-syntax-hash-caret (object-table)
  (set-dispatch-macro-character
   #\# #\^
   (lambda (stream char param)
     (declare (ignore stream char))
     (gethash param object-table))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Close a transaction log.

(defun close-transaction-log (transaction-log)
  (close (log-stream transaction-log)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Log a transaction.
;;;
;;; A transaction can be any object that the application sees fit.

(defun log-transaction (transaction transaction-log)
  (serialize transaction transaction-log)
  (terpri (log-stream transaction-log))
  (finish-output (log-stream transaction-log)))
