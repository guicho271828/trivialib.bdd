#|
  This file is a part of trivialib.bdd project.
  Copyright (c) 2015 Masataro Asai (guicho2.71828@gmail.com)
|#

(in-package :cl-user)
(defpackage :trivialib.bdd.test
  (:use :cl
        :trivialib.bdd
        :fiveam
        :alexandria :trivia))
(in-package :trivialib.bdd.test)



(def-suite :trivialib.bdd)
(in-suite :trivialib.bdd)

(test bdd-xor
  (with-odd-context (:variables '(a b))
    (is (eq (leaf t) (leaf t)))
    (is (eq (leaf nil) (leaf nil)))
    (is (eq (bdd-node 0 (leaf t) (leaf nil))
            (bdd-node 0 (leaf t) (leaf nil))))
    (let ((dd1 (bdd-node 0 (leaf t) (leaf nil)))
          (dd2 (bdd-node 1 (leaf t) (leaf nil))))
      (print (node-apply dd1 dd2 #'bdd-node (lambda (a b) (xor a b)))))))

(test zdd-xor
  (with-odd-context (:variables '(a b))
    (is (eq (leaf t) (leaf t)))
    (is (eq (leaf nil) (leaf nil)))
    (is (eq (zdd-node 0 (leaf t) (leaf nil))
            (zdd-node 0 (leaf t) (leaf nil))))
    (let ((dd1 (zdd-node 0 (leaf t) (leaf nil)))
          (dd2 (zdd-node 1 (leaf t) (leaf nil))))
      (print (node-apply dd1 dd2 #'zdd-node (lambda (a b) (xor a b)))))))

(test env
  (let* ((odd1 (with-odd-context (:variables '(a b))
                 (odd (bdd-node 0 (leaf t) (leaf nil)))))
         (odd2 (with-odd-context (:default odd1)
                 (odd (bdd-node 0 (leaf t) (leaf nil))))))
    (finishes
      (odd-apply odd1 odd2 (lambda (a b) (xor a b)))))
  (let* ((odd1 (with-odd-context (:variables '(a b))
                 (odd (bdd-node 0 (leaf t) (leaf nil)))))
         (odd2 (with-odd-context (:variables '(b a))
                 (odd (bdd-node 0 (leaf t) (leaf nil))))))
    (signals error
      (odd-apply odd1 odd2 (lambda (a b) (xor a b)))))
  (let* ((odd1 (with-odd-context (:variables '(a b))
                 (odd (bdd-node 0 (leaf t) (leaf nil)))))
         (odd2 (with-odd-context (:default odd1 :node-cache (make-hash-table))
                 (odd (bdd-node 0 (leaf t) (leaf nil))))))
    (signals error
      (odd-apply odd1 odd2 (lambda (a b) (xor a b)))))
  (let* ((odd1 (with-odd-context (:variables '(a b))
                 (odd (bdd-node 0 (leaf t) (leaf nil)))))
         (odd2 (with-odd-context (:default odd1 :generator #'zdd-node)
                 (odd (bdd-node 0 (leaf t) (leaf nil))))))
    (signals error
      (odd-apply odd1 odd2 (lambda (a b) (xor a b))))))


(test util
  (with-odd-context (:variables '(a b))
    (let ((odd1 (bdd (unit 0)))
          (odd2 (bdd (unit 1))))
      (finishes
        (print (odd-apply odd1 odd2 (lambda (a b) (xor a b))))))))

(defun gcprint (thing &rest args)
  (let ((*print-circle* t))
    (apply #'print
           (prog1 thing
                  (tg:gc :full t :verbose t))
           args)))

(test compare-zdd-bdd
  ;; Knuth Chapter 4, figure 12 (and 122), rightmost ("kernel")
  ;; represent a set {{1,3,5},{1,4},{2,4,6},{2,5},{3,6}}
  (labels ((o (x) (odd (unit x)))
           (x (x) (odd (!unit x)))
           (add2 (dd1 dd2) (odd-apply dd1 dd2 (lambda (a b) (or a b))))
           (mul2 (dd1 dd2) (odd-apply dd1 dd2 (lambda (a b) (and a b))))
           (add (dd1 dd2 &rest args)
             (if args
                 (apply #'add (add2 dd1 dd2) args)
                 (add2 dd1 dd2)))
           (mul (dd1 dd2 &rest args)
             (if args
                 (apply #'mul (mul2 dd1 dd2) args)
                 (mul2 dd1 dd2))))

    (with-odd-context ()
      ;; 23
      (gcprint
       (add (mul (o 1) (x 2) (o 3) (x 4) (o 5) (x 6))
            (mul (o 1) (x 2) (x 3) (o 4) (x 5) (x 6))
            (mul (x 1) (o 2) (x 3) (o 4) (x 5) (o 6))
            (mul (x 1) (o 2) (x 3) (x 4) (o 5) (x 6))
            (mul (x 1) (x 2) (o 3) (x 4) (x 5) (o 6)))))))
