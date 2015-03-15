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
  :depends-on (:alexandria :trivia)
  :components ((:module "src"
                :components
                ((:file "package"))))
  :description ""
  :in-order-to ((test-op (load-op trivialib.bdd.test))))
