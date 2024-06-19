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

(def-serialize-method ((object standard-object) transaction-log) (id log-stream)
  (format log-stream "#~d![" id)
  (serialize (class-name (class-of object)) transaction-log)
  (loop for info in (save-info object)
	do (handler-case
	       (let* ((reader (cadr info))
		      (value (funcall reader object)))
		 (format log-stream " ")
		 (serialize (car info) transaction-log)
		 (format log-stream " ")
		 (serialize value transaction-log))
	     (unbound-slot ())))
  (format log-stream "]"))

(def-serialize-method ((object pathname) transaction-log) (id log-stream)
  (format log-stream "#~d!" id)
  (prin1 object log-stream)
  )
