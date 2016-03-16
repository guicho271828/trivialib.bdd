
(in-package :trivialib.bdd)

(defstruct dd
  (tag 0 :type fixnum))

(defstruct (leaf (:include dd)) content)

(defstruct (internal (:include dd))
  (true (leaf) :type dd)
  (false (leaf) :type dd))

(defpattern boolean ()
  `(or t nil))



