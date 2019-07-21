;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; This file represents the robot-model-based-agent-program class.
;;;;;
;;;;; Created: 2/24/2019
;;;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Class: robot-model-based-agent-program
;;; Parent class: model-based-agent-program, robot-simple-reflex-agent-program
;;; Slots: none
;;; Description:
;;; 	Represents a Robot Model-based Agent Program.
;;; 	Stores percept and action sequences.
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 2/24/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass robot-model-based-agent-program (model-based-agent-program robot-simple-reflex-agent-program) ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Method: action-
;;; Arguments:
;;; 	- agent-program: a robot-model-based-agent-program
;;; Returns: an action
;;; Description:
;;; 	Produces an action.
;;; 	Always moves forward, unless front sensor is firing, then moves right.
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 2/24/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod action- ((agent-program robot-model-based-agent-program))
	(with-accessors ((front-sensor robot-senses-front-sensor)) (percepts agent-program)
		(if front-sensor
			(make-robot-move-action :direction 'right)
			(make-robot-move-action :direction 'forward))))
