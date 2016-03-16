#|
  This file is a part of trivialib.bdd project.
  Copyright (c) 2015 Masataro Asai (guicho2.71828@gmail.com)
|#

(in-package :cl-user)
(defpackage trivialib.bdd
  (:use :cl :alexandria :trivia)
  (:shadowing-import-from :immutable-struct :ftype)
  (:export
   #:dd
   #:leaf
   #:odd-apply
   #:odd
   #:with-odd-context
   #:bdd
   #:zdd
   #:bdd-apply
   #:zdd-apply))
(in-package :trivialib.bdd)

;; blah blah blah.

