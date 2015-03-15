#|
  This file is a part of trivialib.bdd project.
  Copyright (c) 2015 Masataro Asai (guicho2.71828@gmail.com)
|#

(in-package :cl-user)
(defpackage :trivialib.bdd.test
  (:use :cl
        :trivialib.bdd
        :fiveam
        :alexandria :trivia :trivia.skip))
(in-package :trivialib.bdd.test)



(def-suite :trivialib.bdd)
(in-suite :trivialib.bdd)

(defun merge-bdd (bvs op)
  (reduce (curry #'bdd-apply op) bvs :key #'bdd/bitvec))

(test bdd
  (is (eq t (merge-bdd '(#*1 #*0) 'or)))
  (is (eq nil (merge-bdd '(#*1 #*0) 'and)))
  (is (eq nil (bdd-not t)))
  (is (eq t (bdd-not nil)))
  (is (eq nil (bdd-apply 'and
                         (bdd/bitvec #*0110)
                         (merge-bdd '(#*0001 #*1000) 'or))))

  (is (eq nil (reduce
               (lambda (zdd args) (apply #'zdd-restrict zdd args))
               (list* (merge-bdd '(#*0001 #*1000) 'or)
                      (map 'list #'list (iota 4) (map 'list #'plusp #*0110))))))
  (is (eq nil (bdd-restrict/bitvec (merge-bdd '(#*0001 #*1000) 'or)
                                   #*0110))))

(defun merge-zdd (bvs op)
  (reduce (curry #'zdd-apply op) bvs :key #'zdd/bitvec))

(test zdd
  ;(is (eq t (merge-zdd '(#*1 #*0) 'or)))
  ;(is (eq nil (merge-zdd '(#*1 #*0) 'and)))
  (is (eq nil (zdd-not t)))
  (is (eq t (zdd-not nil)))
  (is (eq nil (zdd-restrict/bitvec (merge-bdd '(#*0001 #*1000) 'or) #*0110))))







