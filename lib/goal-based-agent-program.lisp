;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; This file represents the goal-based-agent-program class.
;;;;;
;;;;; Created: 2/24/2019
;;;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Class: goal-based-agent-program
;;; Parent class: model-based-agent-program
;;; Slots:
;;; 	- test: a function that tests for goal state
;;; 	- agent-meta: the agent-meta of the agent
;;; 	- adder: a function that gives step cost
;;; 	- heuristic: a function that gives heuristic cost
;;; 	- generator: a function that generates new states
;;; Description:
;;; 	Represents a Goal-based Agent Program.
;;; 	Uses local searching with heuristics to explore state space.
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 2/24/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass goal-based-agent-program (model-based-agent-program)
	((test :reader test :initarg :test :initform nil)
	(agent-meta :accessor agent-meta :initarg :agent-meta)
	(adder :reader adder :initarg adder)
	(heuristic :reader heuristic :initarg heuristic)
	(generator :accessor generator :initarg :generator)))
