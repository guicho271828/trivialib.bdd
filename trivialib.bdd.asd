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
               (:file "2.odd")
               (:file "3.bdd")
               (:file "4.zdd"))
  :serial t
  :description "BDD and ZDD implementation using Trivia"
  :in-order-to ((test-op (test-op trivialib.bdd.test))))
