;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; This file represents the robot-solution-performance-measure class.
;;;;;
;;;;; Created: 2/24/2019
;;;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Class: robot-solution-performance-measure
;;; Parent class: performance-measure
;;; Slots: none
;;; Description:
;;; 	Represents a solution performance measure.
;;; 	Measures the path length performance of an agent.
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 2/24/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass robot-solution-performance-measure (performance-measure) ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Method: measure
;;; Arguments:
;;; 	- performance-measure: a robot-solution-performance-measure
;;; 	- args (rest): the arguments (n-expanded and max-size)
;;; Returns: nil
;;; Description:
;;; 	Measures the path length performance of the agent.
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 2/24/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod measure ((performance-measure robot-solution-performance-measure) &rest args)
	(let* ((path (first args))
			(world (second args))
			(goals (third args))
			(location (first path)))
		(format t "Path length: ~a~%" (if (/= (length path) 1)
											(length path)))
		(setf (agent-metas world) '())
		(let* ((goal (min-goal- goals (make-robot-agent-meta :state location) #'manhattan-distance))
				(node (make-instance 'node :label location :adder #'1+ :heuristic (curry #'manhattan-distance goal) :generator (curry #'world-neighbors world)))
				(graph (make-instance 'graph :root node :test (curry #'goal-test goals)))
				(optimal-path (a* graph)))
			(format t "Optimal path length: ~a~%" (if optimal-path
														(length optimal-path))))))
