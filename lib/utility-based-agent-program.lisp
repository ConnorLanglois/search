;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; This file represents the utility-based-agent-program class.
;;;;;
;;;;; Created: 2/24/2019
;;;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Class: utility-based-agent-program
;;; Parent class: goal-based-agent-program
;;; Slots:
;;; 	- world: the world
;;; 	- path: the path
;;; 	- n-expanded: the number of nodes expanded in graph search
;;; 	- max-size: the max number of nodes in frontier in graph search
;;; 	- searched: t if already searched, otherwise nil
;;; Description:
;;; 	Represents a Utility-based Agent Program.
;;; 	Performs graph search to find path to goal.
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 2/24/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass utility-based-agent-program (goal-based-agent-program)
	((world :reader world)
	(path :accessor path :initform '())
	(n-expanded :accessor n-expanded :initform nil)
	(max-size :accessor max-size :initform nil)
	(searched :accessor searched :initform nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Method: percept
;;; Arguments:
;;; 	- agent-program: a utility-based-agent-program
;;; 	- percepts: the current percepts
;;; Returns: t if not already searched, otherwise nil
;;; Description:
;;; 	Performs graph search to find path to goal.
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 2/24/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod percept ((agent-program utility-based-agent-program) percepts)
	(call-next-method)
	(with-accessors ((searched searched)) agent-program
		(when (not searched)
			(seek agent-program)
			(setf searched t))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Method: action-
;;; Arguments:
;;; 	- agent-program: a utility-based-agent-program
;;; Returns: an action
;;; Description:
;;; 	Gets the action from the next state.
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 2/24/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod action- ((agent-program utility-based-agent-program))
	(with-accessors ((path path)) agent-program
		(if (= (length path) 0)
			(make-nop-action)
			(action-- agent-program (pop path)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Method: seek
;;; Arguments:
;;; 	- agent-program: a utility-based-agent-program
;;; Returns: the max size of the frontier
;;; Description:
;;; 	Performs graph search to find path to goal.
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 2/24/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod seek ((agent-program utility-based-agent-program))
	(with-accessors ((path path) (n-expanded n-expanded) (max-size max-size) (alg alg)) agent-program
		(multiple-value-bind (alg-path alg-n-expanded alg-max-size) (funcall alg (graph agent-program))
			(setf path (rest alg-path))
			(setf n-expanded alg-n-expanded)
			(setf max-size alg-max-size))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Method: make-generator
;;; Arguments:
;;; 	- agent-program: a utility-based-agent-program
;;; Returns: a generator function
;;; Description:
;;; 	Makes a generator (invoked when world updated).
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 2/24/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod make-generator ((agent-program utility-based-agent-program))
	(generator agent-program))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Method: setf world
;;; Arguments:
;;; 	- new-world: the new world
;;; 	- agent-program: a utility-based-agent-program
;;; Returns: the new generator
;;; Description:
;;; 	Stores the new world and generator.
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 2/24/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod (setf world) (new-world (agent-program utility-based-agent-program))
	(setf (slot-value agent-program 'world) new-world)
	(setf (generator agent-program) (make-generator agent-program)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Generic function: action--
;;; Arguments:
;;; 	- utility-based-agent-program: a utility-based-agent-program
;;; 	- state: the state for which to produce an action
;;; Returns: an action
;;; Description:
;;; 	Gets the action from the state.
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 2/24/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric action-- (utility-based-agent-program state))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Generic function: graph
;;; Arguments:
;;; 	- utility-based-agent-program: an utility-based-agent-program
;;; Returns: a graph
;;; Description:
;;; 	Makes a graph to search.
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 2/24/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric graph (utility-based-agent-program))
