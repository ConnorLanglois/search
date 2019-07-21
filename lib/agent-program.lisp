;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; This file represents the agent-program class.
;;;;;
;;;;; Created: 2/24/2019
;;;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Class: agent-program
;;; Parent class: none
;;; Slots:
;;; 	- percepts: the current percepts
;;; Description:
;;; 	Represents the agent program of the agent.
;;; 	Receives percepts and produces action.
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 2/24/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass agent-program ()
	((percepts :accessor percepts)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Method: percept
;;; Arguments:
;;; 	- agent-program: an agent-program
;;; 	- percepts: the percepts to be stored as the current percepts
;;; Returns: the percepts
;;; Description:
;;; 	Stores the current percepts.
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 2/24/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod percept ((agent-program agent-program) percepts)
	(setf (percepts agent-program) percepts))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Generic function: action
;;; Arguments:
;;; 	- agent-program: an agent-program
;;; Returns: an action
;;; Description:
;;; 	Produces an action.
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 2/24/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric action (agent-program))
