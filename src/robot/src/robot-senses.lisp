;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; This file represents the robot-senses structure.
;;;;;
;;;;; Created: 2/24/2019
;;;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Structure: robot-senses
;;; Parent structure: senses
;;; Slots:
;;; 	- front-sensor: whether the front-sensor is firing
;;; 	- front-bump: whether the front-bump is fired on the last move
;;; 	- left-bump: whether the left-bump is fired on the last move
;;; 	- right-bump: whether the right-bump is fired on the last move
;;; 	- rear-bump: whether the rear-bump is fired on the last move
;;; Description:
;;; 	Represents senses of agent.
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 2/24/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (robot-senses (:include senses))
	(front-sensor nil :read-only t)
	(front-bump nil :read-only t)
	(left-bump nil :read-only t)
	(right-bump nil :read-only t)
	(rear-bump nil :read-only t))
