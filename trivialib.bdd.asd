#|
  This file is a part of trivialib.bdd project.
  Copyright (c) 2015 Masataro Asai (guicho2.71828@gmail.com)
|#

#|
  Author: Masataro Asai (guicho2.71828@gmail.com)
|#



(in-package :cl-user)
(defpackage trivialib.bdd-asd
  (:use :cl :asdf))
(in-package :trivialib.bdd-asd)


(defsystem trivialib.bdd
  :version "0.1"
  :author "Masataro Asai"
  :mailto "guicho2.71828@gmail.com"
  :license "LLGPL"
  :depends-on (:alexandria :trivia :trivial-garbage :immutable-struct)
  :pathname "src/"
  :components ((:file "0.package")
               (:file "1.struct")
               ;; (:file "2.hash")
               (:file "3.container")
               ;; (:file "4.bdd")
               ;; (:file "5.zdd")
               )
  :description "BDD and ZDD implementation"
  :in-order-to ((test-op (test-op trivialib.bdd.test))))
