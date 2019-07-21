;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; This file represents the agent-sim structure.
;;;;;
;;;;; Created: 2/28/2019
;;;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Structure: agent-sim
;;; Parent structure: none
;;; Slots:
;;; 	- test: a function that tests for goal state
;;; 	- performance-measures: a list of performance measures
;;; 	- path: a list of states
;;; Description:
;;; 	Represents agent from point of view of simulator.
;;; 	Meant to be inherited from.
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 2/28/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct agent-sim
	(test nil :read-only t)
	(performance-measures '() :read-only t)
	(path '()))
