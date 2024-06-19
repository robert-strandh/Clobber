(cl:in-package #:common-lisp-user)

(defpackage #:clobber-test
  (:use #:common-lisp #:clobber #:clobber-internal)
  (:export #:run-tests))
