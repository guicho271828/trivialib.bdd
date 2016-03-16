
(in-package :trivialib.bdd)

(defun bdd (root &optional (variables *variables*) (node-cache *node-cache*))
  (odd root variables node-cache #'bdd-node))

(defun zdd (root &optional (variables *variables*) (node-cache *node-cache*))
  (odd root variables node-cache #'zdd-node))

(defun unit (variable)
  (bdd-node variable (leaf t) (leaf nil)))



