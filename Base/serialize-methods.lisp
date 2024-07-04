(cl:in-package #:clobber-internal)

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

(defmethod serialize ((object standard-generic-function) transaction-log)
  (let ((name (closer-mop:generic-function-name object)))
    (serialize name transaction-log)))

(def-serialize-method ((object cons) transaction-log) (id% log-stream)
  (format log-stream "#~d!(" id%)
  (serialize (car object) transaction-log)
  (format log-stream " . ")
  (serialize (cdr object) transaction-log)
  (format log-stream ")"))

;;; FIXME: add serialization of arbitrary arrays.

(def-serialize-method ((object vector) transaction-log) (id log-stream)
  (format log-stream "#~d!#(" id)
  (loop for element across object
        do (serialize element transaction-log)
           (format log-stream " "))
  (format log-stream ")"))

(def-serialize-method ((object string) transaction-log) (id log-stream)
  (format log-stream "#~d!\"" id)
  (loop for char across object
        do (cond ((eql char #\")
                  (format log-stream "\\\""))
                 ((eql char #\\)
                  (format log-stream "\\\\"))
                 (t
                  (princ char log-stream))))
  (format log-stream "\""))

(defun serialize-pair (initarg value transaction-log
		       &aux (log-stream (log-stream transaction-log)))
  (format log-stream " ")
  (serialize initarg transaction-log)
  (format log-stream " ")
  (serialize value transaction-log))

(def-serialize-method ((object standard-object) transaction-log) (id log-stream)
  (format log-stream "#~d![" id)
  (serialize (class-name (class-of object)) transaction-log)
  (loop for info in (save-info object)
	do (handler-case
	       (let* ((initarg (car info))
		      (reader (cadr info))
		      (value (funcall reader object)))
                 (serialize-pair initarg value transaction-log))
	     (unbound-slot ())))
  (format log-stream "]"))

(defun hash-table-to-alist (hash-table)
  (let ((result '()))
    (maphash (lambda (key val)
	       (push (cons key val) result))
	     hash-table)
    (nreverse result)))

(def-serialize-method ((object hash-table) transaction-log) (id log-stream)
  (format log-stream "#~d![" id)
  (serialize 'hash-table transaction-log)
  (serialize-pair :test (hash-table-test object) transaction-log)
  (serialize-pair :elements (hash-table-to-alist object) transaction-log)
  (serialize-pair :size (hash-table-size object) transaction-log)
  (serialize-pair :rehash-size (hash-table-rehash-size object) transaction-log)
  (serialize-pair :rehash-threshold (hash-table-rehash-threshold object) transaction-log)
  (format log-stream "]"))

(def-serialize-method ((object pathname) transaction-log) (id log-stream)
  (format log-stream "#~d!" id)
  (prin1 object log-stream))


