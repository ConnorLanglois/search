;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; This file represents the node class.
;;;;;
;;;;; Created: 2/24/2019
;;;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Class: node
;;; Parent class: none
;;; Slots:
;;; 	- label: the label
;;; 	- parent: the parent
;;; 	- cost: the cumulative cost
;;; 	- adder: a function that gives step cost
;;; 	- heuristic: a function that gives heuristic cost
;;; 	- heuristic-value: the value calculated from the heuristic function
;;; 	- generator: a function that generates new states
;;; Description:
;;; 	Represents the agent program of the agent.
;;; 	Receives percepts and produces action.
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 2/24/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass node ()
	((label :reader label :initarg :label)
	(parent :reader parent :initarg :parent :initform nil)
	(cost :reader cost :initarg :cost :initform 0)
	(adder :reader adder :initarg :adder)
	(heuristic :reader heuristic :initarg :heuristic)
	(heuristic-value :reader heuristic-value :initarg :heuristic-value)
	(generator :reader generator :initarg :generator)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Method: successors
;;; Arguments:
;;; 	- node: a node
;;; Returns: a list of node
;;; Description:
;;; 	Generates the successors
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 2/24/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod successors ((node node))
	(with-accessors ((label label) (cost cost) (adder adder) (heuristic heuristic) (generator generator)) node
		(mapcar (lambda (label) (make-instance 'node :label label :parent node :cost (funcall adder cost) :heuristic heuristic :heuristic-value (funcall heuristic label) :generator generator :adder adder)) (funcall generator label))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Method: path
;;; Arguments:
;;; 	- node: a node
;;; Returns: a list of states
;;; Description:
;;; 	Generates the path of the search
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 2/24/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod path ((node node))
	(with-accessors ((label label) (parent parent)) node
		(append (when parent
					(path parent)) (list node))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Function: node=
;;; Arguments:
;;; 	- node1: a node
;;; 	- node2: a node
;;; Returns: a list of states
;;; Description:
;;; 	Checks if two nodes are equal
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 2/24/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun node= (node1 node2)
	(equalp (label node1) (label node2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Function: min-node
;;; Arguments:
;;; 	- node1: a node
;;; 	- node2: a node
;;; Returns: the "lower" node
;;; Description:
;;; 	Determines the "lower" of the two nodes
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 2/24/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun min-node (node1 node2)
	(if (min-node-p node1 node2)
		node1
		node2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Function: min-node
;;; Arguments:
;;; 	- node1: a node
;;; 	- node2: a node
;;; Returns: whether the first node is "lower" than the second
;;; Description:
;;; 	Checks if the first node is "lower" than the second
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 2/24/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun min-node-p (node1 node2)
	(< (+ (cost node1) (heuristic-value node1)) (+ (cost node2) (heuristic-value node2))))
