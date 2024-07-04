(cl:in-package #:clobber-internal)

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

;;; There's a pattern of getting an ID for an object, and either printing
;;; a cycle marker if the ID was seen before (i.e. the object was printed before)
;;; or printing the object.
;;; Only the latter is different in each use of the pattern.

(defun serialize-common (object transaction-log fn)
  (with-accessors ((object-id-table object-id-table)
                   (next-object-id next-object-id)
                   (log-stream log-stream))
      transaction-log
    (let ((id (gethash object object-id-table)))
      (if (null id)
          (progn
            (setf id (incf next-object-id))
            (setf (gethash object object-id-table) id)
	    (funcall fn object id log-stream transaction-log))
          (format log-stream "#~d^" id)))))

(defmacro with-serialize-common ((object transaction-log id log-stream) &body body)
  `(serialize-common ,object transaction-log
    (lambda (,object ,id ,log-stream ,transaction-log)
      (declare (ignorable ,id ,log-stream ,transaction-log))
      ,@body)))

(defmacro def-serialize-method (((object class) transaction-log) (id log-stream)  &body body)
  `(defmethod serialize ((,object ,class) ,transaction-log)
     (with-serialize-common (,object ,transaction-log ,id ,log-stream)
       ,@body)))

(defmacro with-string-transaction-log (symbol &body body)
  (let ((output-stream (gensym "OUTPUT-STREAM")))
    `(with-output-to-string (,output-stream)
       (let ((,symbol (make-transaction-log ,output-stream (make-hash-table))))
	 ,@body))))

(defun serialize-to-string (object)
  (with-string-transaction-log transaction-log    
    (serialize object transaction-log)
    (commit transaction-log)))

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

(defun make-a-hash-table (&key (test #'eql) elements
			    size rehash-size rehash-threshold)
  (let ((hash-table (apply #'make-hash-table
			   :test test
			   (append
			    (unless (null size)
			      (list :size size))
			    (unless (null rehash-size)
			      (list :rehash-size rehash-size))
			    (unless (null rehash-threshold)
			      (list :rehash-threshold rehash-threshold))))))
    (loop :for (key . value) :in elements
          :do (setf (gethash key hash-table) value))
    hash-table))

;;; specification==(object-type :initarg1 initval1 :initarg2 initval2 ...)
(defun fn-and-args (specification)
  (case (car specification) 
    ('hash-table (values #'make-a-hash-table (cdr specification)))
    (otherwise (values #'make-instance specification))))

(defun %set-syntax-left-bracket ()
  (set-macro-character
   #\[
   (lambda (stream char)
     (declare (ignore char))
     (multiple-value-bind (fn args) (fn-and-args (read-delimited-list #\] stream t))
       (apply fn args)))))

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
