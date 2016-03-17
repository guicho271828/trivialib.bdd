
(in-package :trivialib.bdd)

#+trivialib.bdd.debug
(defvar *id* 0)

(defstruct leaf
  "Leaf node of a decision diagram"
  (content nil)
  #+trivialib.bdd.debug
  (id (incf *id*)))

(declaim (type hash-table *leaf-cache*))
(defvar *leaf-cache* (tg:make-weak-hash-table :weakness :value :test #'eql)
  "hash table to look up a leaf node of a thing.")

(defun leaf (thing)
  "Return the leaf node in the hash table *leaf-cache*, creating an instance when required"
  (ensure-gethash thing *leaf-cache* (make-leaf :content thing)))

(defstruct node
  "lightweight node in Decision Diagram.
VARIABLE: an integer representing the index of a variable. cf. VARIABLES slot in structure DD
HI,LO: hi/lo pointer"
  (variable 0 :type fixnum)
  (hi (leaf nil) :type (or node leaf))
  (lo (leaf nil) :type (or node leaf))
  #+trivialib.bdd.debug
  (id (incf *id*)))

(defvar *node-cache*)
(declaim (type hash-table *node-cache*))

(setf (documentation '*node-cache* 'variable)
      "Hash table to look up in order to avoid the creation of redundunt nodes.
 This cache is specific to the ODD context defined by the pair of variables and operation(BDD-APPLY or ZDD-APPLY).")

(defvar *variables*)
(declaim (type sequence *variables*))

(setf (documentation '*variables* 'variable)
      "ODD variables in the current context.")


(defpattern node (&optional variable hi lo)
  `(structure node :variable ,variable :hi ,hi :lo ,lo))


