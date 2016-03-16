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
   #:node-apply
   #:bdd-node
   #:zdd-node
   ;; managing contexts
   #:odd
   #:odd-apply
   #:with-odd-context
   #:call-with-odd-context
   ;; utility
   #:bdd
   #:zdd
   #:bdd-apply
   #:zdd-apply
   #:unit
   #:!unit))
(in-package :trivialib.bdd)

;; blah blah blah.

