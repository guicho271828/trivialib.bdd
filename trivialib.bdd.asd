#|
  This file is a part of trivialib.bdd project.
  Copyright (c) 2015 Masataro Asai (guicho2.71828@gmail.com)
|#

#|
  Author: Masataro Asai (guicho2.71828@gmail.com)
|#


(defsystem trivialib.bdd
  :version "0.1"
  :author "Masataro Asai"
  :mailto "guicho2.71828@gmail.com"
  :license "LLGPL"
  :depends-on (:alexandria :trivia :trivial-garbage :immutable-struct)
  :pathname "src/"
  :components ((:file "0.package")
               (:file "1.struct")
               (:file "3.container")
               (:file "4.util"))
  :serial t
  :description "BDD and ZDD implementation"
  :in-order-to ((test-op (test-op trivialib.bdd.test))))
