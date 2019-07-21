;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Function: make-agent-programs
;;; Arguments:
;;; 	- agent-programs: a list of agent-program symbols
;;; 	- nested-goals (optional): a list of list of goals
;;; Description:
;;; 	Makes agent-programs with goals.
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 3/3/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-agent-programs (agent-programs &optional nested-goals)
	(mapcar #'make-agent-program agent-programs nested-goals))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Function: make-agent-program
;;; Arguments:
;;; 	- agent-program: an agent-program symbol
;;; 	- goals (optional): a list of goals
;;; 	- world (optional): the world
;;; Description:
;;; 	Makes agent-program with goals.
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 3/3/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-agent-program (agent-program &optional goals world)
	(apply #'make-instance agent-program (cond
											((subtypep agent-program 'goal-based-agent-program) (list :goals goals))
											((subtypep agent-program 'utility-based-agent-program) (list :goals goals :world world)))))
