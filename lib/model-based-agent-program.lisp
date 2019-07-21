;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; This file represents the model-based-agent-program class.
;;;;;
;;;;; Created: 2/27/2019
;;;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Class: model-based-agent-program
;;; Parent class: none
;;; Slots:
;;; 	- percept-sequence: the percept history
;;; 	- action-sequence: the action history
;;; Description:
;;; 	Represents a Model-based Agent Program.
;;; 	Stores percept and action sequences.
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 2/27/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass model-based-agent-program (simple-reflex-agent-program)
	((percept-sequence :accessor percept-sequence :initform '())
	(action-sequence :accessor action-sequence :initform '())))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Method: percept
;;; Arguments:
;;; 	- agent-program: a model-based-agent-program
;;; 	- percepts: the percepts to be stored as the current percepts
;;; Returns: the percept-sequence
;;; Description:
;;; 	Stores the percepts into the percept sequence.
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 2/27/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod percept ((agent-program model-based-agent-program) percepts)
	(call-next-method)
	(with-accessors ((percept-sequence percept-sequence)) agent-program
		(if percept-sequence
			(push percepts (cdr (last percept-sequence)))
			(push percepts percept-sequence))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Method: action
;;; Arguments:
;;; 	- agent-program: a model-based-agent-program
;;; Returns: an action
;;; Description:
;;; 	Produces an action and stores into action sequence.
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 2/27/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod action ((agent-program model-based-agent-program))
	(with-accessors ((action-sequence action-sequence)) agent-program
		(let ((action (action- agent-program)))
			(if action-sequence
				(push action (cdr (last action-sequence)))
				(push action action-sequence))
			action)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Generic function: action-
;;; Arguments:
;;; 	- model-based-agent-program: a model-based-agent-program
;;; Returns: an action
;;; Description:
;;; 	Produces an action.
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 2/27/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric action- (model-based-agent-program))
