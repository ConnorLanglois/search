;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; This file represents the robot-utility-based-agent-program class.
;;;;;
;;;;; Created: 2/25/2019
;;;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Class: robot-utility-based-agent-program
;;; Parent class: utility-based-agent-program, robot-goal-based-agent-program
;;; Slots: none
;;; Description:
;;; 	Represents a Robot Utility-based Agent Program.
;;; 	Performs graph search to find path to goal.
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 2/25/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass robot-utility-based-agent-program (utility-based-agent-program robot-goal-based-agent-program) ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Method: action--
;;; Arguments:
;;; 	- agent-program: a robot-utility-based-agent-program
;;; 	- new-location: the new location
;;; Returns: an action
;;; Description:
;;; 	Makes an action from the new location to which to move.
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 2/25/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod action-- ((agent-program robot-utility-based-agent-program) new-location)
	(with-accessors ((location robot-agent-meta-state) (heading robot-agent-meta-heading)) (agent-meta agent-program)
		(make-robot-move-action :direction (bearing-to-direction (bearing location new-location) heading))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Method: graph
;;; Arguments:
;;; 	- agent-program: a robot-utility-based-agent-program
;;; Returns: a graph
;;; Description:
;;; 	Makes a graph to search.
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 2/25/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod graph ((agent-program robot-utility-based-agent-program))
	(with-accessors ((goals goals) (agent-meta agent-meta) (adder adder) (heuristic heuristic) (generator generator)) agent-program
		(with-accessors ((location robot-agent-meta-state)) agent-meta
			(let* ((goal (min-goal agent-program))
					(node (make-instance 'node :label location :adder adder :heuristic (curry heuristic goal) :generator generator)))
						(make-instance 'graph :root node :test (curry #'goal-test goals))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Method: make-generator
;;; Arguments:
;;; 	- agent-program: a robot-utility-based-agent-program
;;; Returns: a generator function
;;; Description:
;;; 	Makes a generator (invoked when world updated).
;;; 	Curries the world with world-neighbors.
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 2/25/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod make-generator ((agent-program robot-utility-based-agent-program))
	(curry #'world-neighbors (world agent-program)))
