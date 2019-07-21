;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; This file represents the robot-goal-based-agent-program class.
;;;;;
;;;;; Created: 2/27/2019
;;;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Class: robot-goal-based-agent-program
;;; Parent class: goal-based-agent-program, robot-model-based-agent-program
;;; Slots:
;;; 	- goals: the goals of the agent
;;; 	- coordinate-sequence: the coordinate history
;;; 	- adder: a function that gives step cost
;;; 	- heuristic: a function that gives heuristic cost
;;; 	- generator: a function that generates new states
;;; Description:
;;; 	Represents a Robot Goal-based Agent Program.
;;; 	Uses local searching with heuristics to explore state space.
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 2/27/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass robot-goal-based-agent-program (goal-based-agent-program robot-model-based-agent-program)
	((goals :reader goals :initarg :goals :initform '())
	(coordinate-sequence :accessor coordinate-sequence :initform '())
	(adder :reader adder :initform #'1+)
	(heuristic :reader heuristic :initform #'manhattan-distance)
	(generator :accessor generator :initform #'neighbors)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Method: initialize-instance
;;; Arguments:
;;; 	- agent-program: a robot-goal-based-agent-program
;;; 	- goals (key): the goals
;;; Returns: the robot-goal-based-agent-program
;;; Description:
;;; 	Initializes the robot-goal-based-agent-program.
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 2/27/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod initialize-instance :around ((agent-program robot-goal-based-agent-program) &key goals)
	(call-next-method agent-program :test (curry #'goal-test goals) :goals goals))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Method: min-goal
;;; Arguments:
;;; 	- agent-program: a robot-goal-based-agent-program
;;; Returns: a coordinate
;;; Description:
;;; 	Gets the minimum goal of the agent.
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 2/27/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod min-goal ((agent-program robot-goal-based-agent-program))
	(with-accessors ((goals goals) (agent-meta agent-meta) (heuristic heuristic)) agent-program
		(min-goal- goals agent-meta heuristic)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Method: action-
;;; Arguments:
;;; 	- agent-program: a robot-goal-based-agent-program
;;; Returns: an action
;;; Description:
;;; 	Produces an action.
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 2/27/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod action- ((agent-program robot-goal-based-agent-program))
	(with-accessors ((goals goals) (agent-meta agent-meta) (adder adder) (heuristic heuristic) (generator generator)) agent-program
		(with-accessors ((location robot-agent-meta-state)) agent-meta
			(let* ((goal (min-goal agent-program))
					(node (make-instance 'node :label location :adder adder :heuristic (curry heuristic goal) :generator generator)))
				(action-- agent-program node)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Method: action--
;;; Arguments:
;;; 	- agent-program: a robot-goal-based-agent-program
;;; 	- node: a node
;;; Returns: an action
;;; Description:
;;; 	Produces an action.
;;; 	Ranks four neighbors around agent into non-decreasing order based on
;;; 	their heuristic.
;;; 	Filters out already-moved-to coordinates.
;;; 	Chooses minimum neighbor and moves to it.
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 2/27/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod action-- ((agent-program robot-goal-based-agent-program) node)
	(with-accessors ((percepts percepts) (goals goals) (agent-meta agent-meta) (coordinate-sequence coordinate-sequence) (test test)) agent-program
		(with-accessors ((location robot-agent-meta-state) (heading robot-agent-meta-heading)) agent-meta
			(let ((successors (sort (successors node) #'min-node-p)))
				(setf successors (remove-if (lambda (successor) (member (label successor) coordinate-sequence :test #'equalp)) successors))
				(if (or (funcall test location) (= (length successors) 0))
					(make-nop-action)
					(let* ((direction (bearing-to-direction (bearing location (label (first successors))) heading))
							(coordinate (direction-to-new-coordinate direction heading location)))
						(if coordinate-sequence
							(push coordinate (cdr (last coordinate-sequence)))
							(push coordinate coordinate-sequence))
						(make-robot-move-action :direction direction)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Function: goal-test
;;; Arguments:
;;; 	- goals: the goals of the agent
;;; 	- location: the location of the agent
;;; Returns: whether the agent is located at one of its goals
;;; Description:
;;; 	Checks if the agent is located at one of its goals.
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 2/27/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun goal-test (goals location)
	(member location goals :test #'equalp))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Function: manhattan-distance
;;; Arguments:
;;; 	- coordinate1: a coordinate
;;; 	- coordinate2: a coordinate
;;; Returns: the manhattan distance between them
;;; Description:
;;; 	Calculates the manhattan distance between two coordinates.
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 2/27/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun manhattan-distance (coordinate1 coordinate2)
	(let ((coordinate (coordinate- coordinate1 coordinate2)))
		(+ (abs (coordinate-x coordinate)) (abs (coordinate-y coordinate)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Function: min-goal-
;;; Arguments:
;;; 	- goals: a list of goals
;;; 	- agent-meta: an agent-meta
;;; 	- heuristic: a heuristic
;;; Returns: the minimum goal
;;; Description:
;;; 	Gets the minimum goal (based on the heuristic) of a list of goals.
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 2/27/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun min-goal- (goals agent-meta heuristic)
	(with-accessors ((location robot-agent-meta-state)) agent-meta
		(car (minimum (pairlis goals (mapcar (lambda (goal) (funcall heuristic goal location)) goals)) :key #'cdr))))
