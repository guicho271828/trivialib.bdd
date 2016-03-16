#|
  This file is a part of trivialib.bdd project.
  Copyright (c) 2015 Masataro Asai (guicho2.71828@gmail.com)
|#


(in-package :cl-user)
(defpackage trivialib.bdd.test-asd
  (:use :cl :asdf))
(in-package :trivialib.bdd.test-asd)


(defsystem trivialib.bdd.test
  :author "Masataro Asai"
  :license "LLGPL"
  :depends-on (:trivialib.bdd
               :fiveam)
  :components ((:module "t"
                :components
                ((:file "package"))))
  :perform (test-op :after (op c) (eval (read-from-string "(5am:run :trivialib.bdd)"))))
