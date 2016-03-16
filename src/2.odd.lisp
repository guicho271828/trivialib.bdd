
(in-package :trivialib.bdd)

(immutable-struct:defstruct odd
  "Structure representing a whole Decision Diagram.
Holds the context information such as variables and node-cache.
For different variable ordering, ODDs are incompatible."
  (root (leaf nil) :type (or node leaf))
  (variables *variables* :type sequence)
  (node-cache *node-cache* :type hash-table))

(defmacro with-odd-context ((&key default variables node-cache
                                  &allow-other-keys) &body body)
  "Execute BODY in a dynamic environment where *VARIABLES* , *NODE-CACHE* and *GENERATOR* are set.
When DEFAULT is specified, its values are used by default, but are superseded by the specified ones."
  `(call-with-odd-context ,default ,variables ,node-cache (lambda () ,@body)))

(defun call-with-odd-context (default variables node-cache body-fn)
  (match default
    ((odd :variables vs :node-cache nc)
     (let ((*variables* (or variables vs))
           (*node-cache* (or node-cache nc)))
       (funcall body-fn)))
    (_
     (let ((*variables* variables)
           (*node-cache* (or node-cache (tg:make-weak-hash-table :weakness :value :test #'equalp))))
       (funcall body-fn)))))

(defun-ematch* odd-compatible-p (odd1 odd2)
  (((odd :variables vars1
         :node-cache node-cache1)
    (odd :variables vars2
         :node-cache node-cache2))
   (and
    ;; (eq (type-of odd1) (type-of odd2))
    (eq node-cache1 node-cache2)
    (equalp vars1 vars2))))

(defun-ematch* odd-apply (odd1 odd2 operation)
  (((odd root variables node-cache) (odd :root root2))
   (assert (odd-compatible-p odd1 odd2) nil "ODD ~a and ~a are incompatible!" odd1 odd2)
   (odd (funcall operation root root2) variables node-cache)))

