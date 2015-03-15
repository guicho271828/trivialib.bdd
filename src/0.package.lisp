#|
  This file is a part of trivialib.bdd project.
  Copyright (c) 2015 Masataro Asai (guicho2.71828@gmail.com)
|#

(in-package :cl-user)
(defpackage trivialib.bdd
  (:use :cl :alexandria :trivia)
  (:shadowing-import-from :immutable-struct :defstruct :ftype)
  (:export
   #:bdd
   #:bdd-apply
   #:bdd-restrict
   #:bdd-not
   #:bdd-xor
   #:bdd/bitvec
   #:bdd-or
   #:bdd-and
   #:zdd-apply
   #:zdd-hash
   #:zdd-restrict
   #:zdd-xor
   #:zdd-and
   #:zdd-or
   #:zdd/bitvec
   #:zdd-not
   #:bdd-restrict/bitvec
   #:zdd-restrict/bitvec))
(in-package :trivialib.bdd)

;; blah blah blah.

