
(in-package :trivialib.bdd)

;; use t/nil, not 0/1

(defstruct dd
  (tag 0 :type fixnum)
  (true t :type (or boolean dd))
  (false nil :type (or boolean dd)))

(defpattern dd (&optional tag true false)
  `(structure dd (tag ,tag) (true ,true) (false ,false)))

(deftype dd? ()
  `(or boolean dd))

(defpattern = (arg)
  (with-gensyms (m)
    `(guard ,m (= ,m ,arg))))
(defpattern < (arg)
  (with-gensyms (m)
    `(guard ,m (< ,m ,arg))))
(defpattern > (arg)
  (with-gensyms (m)
    `(guard ,m (> ,m ,arg))))

(defpattern boolean ()
  `(or t nil))

(deftype op ()
  `(or (eql and) (eql or) (eql xor)))


