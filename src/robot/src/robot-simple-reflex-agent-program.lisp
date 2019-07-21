;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; This file represents the robot-simple-reflex-agent-program class.
;;;;;
;;;;; Created: 2/24/2019
;;;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Class: robot-simple-reflex-agent-program
;;; Parent class: simple-reflex-	agent-program
;;; Slots: none
;;; Description:
;;; 	Represents a Simple Reflex Agent Program.
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 2/24/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass robot-simple-reflex-agent-program (simple-reflex-agent-program) ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Method: action
;;; Arguments:
;;; 	- agent-program: a robot-simple-reflex-agent-program
;;; Returns: an action
;;; Description:
;;; 	Produces an action.
;;; 	Always moves forward, unless front sensor is firing, then moves right.
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 2/24/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod action ((agent-program robot-simple-reflex-agent-program))
	(with-accessors ((front-sensor robot-senses-front-sensor)) (percepts agent-program)
		(if front-sensor
			(make-robot-move-action :direction 'right)
			(make-robot-move-action :direction 'forward))))
