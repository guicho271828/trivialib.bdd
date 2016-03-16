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
         (odd2 (with-odd-context (:odd odd1)
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
         (odd2 (with-odd-context (:odd odd1 :node-cache (make-hash-table))
                 (odd (bdd-node 0 (leaf t) (leaf nil))))))
    (signals error
      (odd-apply odd1 odd2 (lambda (a b) (xor a b)))))
  (let* ((odd1 (with-odd-context (:variables '(a b))
                 (odd (bdd-node 0 (leaf t) (leaf nil)))))
         (odd2 (with-odd-context (:odd odd1 :generator #'zdd-node)
                 (odd (bdd-node 0 (leaf t) (leaf nil))))))
    (signals error
      (odd-apply odd1 odd2 (lambda (a b) (xor a b))))))

