;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; This file represents the robot-move-action structure.
;;;;;
;;;;; Created: 2/25/2019
;;;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Structure: robot-move-action
;;; Parent structure: action
;;; Slots:
;;; 	- direction: a direction (forward, left, right, backward)
;;; Description:
;;; 	Represents move-agent action by agent.
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 2/25/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (robot-move-action (:include action))
	(direction 'forward :read-only t))
