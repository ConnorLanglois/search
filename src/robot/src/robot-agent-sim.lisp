;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; This file represents the robot-agent-sim structure.
;;;;;
;;;;; Created: 2/28/2019
;;;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Structure: robot-agent-sim
;;; Parent structure: agent-sim
;;; Slots:
;;; 	- goals: the goals of the agent
;;; 	- prior-bump: the prior bump of the agent
;;; Description:
;;; 	Represents agent from point of view of simulator.
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 2/28/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (robot-agent-sim (:include agent-sim))
	(goals '() :read-only t)
	(prior-bump '()))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Function: make-agent-sims
;;; Arguments:
;;; 	- goals (key): the goals of the agents
;;; 	- locations (key): the locations of the agents
;;; 	- performance-measures (key): the performance measures of the agents
;;; Returns: a list of agent-sim
;;; Description:
;;; 	Makes a list of agent-sims.
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 2/24/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-agent-sims (&key goals locations performance-measures)
	(mapcar #'make-agent-sim- goals locations performance-measures))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Function: make-agent-sim-
;;; Arguments:
;;; 	- goals-: the goals of the agent
;;; 	- location: the location of the agent
;;; 	- performance-measures-: the performance measures of the agent
;;; Returns: an agent-sim
;;; Description:
;;; 	Makes an agent-sim.
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 2/24/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-agent-sim- (goals- location performance-measures-)
	(make-robot-agent-sim :test (curry #'goal-test goals-) :path (list location) :performance-measures performance-measures- :goals goals-))
