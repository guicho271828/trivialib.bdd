#|
  This file is a part of trivialib.bdd project.
  Copyright (c) 2015 Masataro Asai (guicho2.71828@gmail.com)
|#

(in-package :cl-user)
(defpackage trivialib.bdd
  (:use :cl :alexandria :trivia)
  (:shadowing-import-from :immutable-struct :ftype)
  (:export
   #:leaf
   #:node
   ;; managing contexts
   #:odd
   #:odd-apply
   #:with-odd-context
   #:call-with-odd-context
   ;; bdd
   #:bdd
   #:bdd-node
   #:bdd-apply
   #:unit
   #:!unit
   ;; zdd
   #:zdd
   #:zdd-node
   #:zdd-apply
   #:change
   #:singleton
   #:dont-care
   #:make-set
   #:make-family))
(in-package :trivialib.bdd)

;; blah blah blah.

