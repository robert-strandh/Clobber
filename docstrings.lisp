(cl:in-package #:clobber)

(setf
 (documentation #'open-transaction-log 'function)
 "Load transaction log from FILENAME and return an instance of `clobber:transaction-log'.

FUNCTION should accept a single argument, a transaction object (see
`clobber:load-transaction-log').")

(setf
 (documentation #'load-transaction-log 'function)
 "Read transaction log from FILENAME, calling FUNCTION for each transaction.

FUNCTION should accept a single argument, a transaction object.

OBJECT-TABLE should be a hash table.")

(setf
 (documentation #'define-save-info 'function)
 "Define SAVE-INFO as data to serialize for instances of TYPE.

Each form in SAVE-INFO should be a list of two elements -

(INITARG ACCESSOR)

where INITARG and ACCESSOR should correspond to a slot for TYPE.")
