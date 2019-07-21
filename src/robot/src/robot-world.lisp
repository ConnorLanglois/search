;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; This file represents the robot-world class.
;;;;;
;;;;; Created: 2/24/2019
;;;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Class: robot-world
;;; Parent class: grid-world
;;; Slots:
;;; 	- obstacles: the obstacles of the world
;;; Description:
;;; 	Represents the grid-based world.
;;; 	Contains obstacles for the agent to avoid.
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 2/24/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass robot-world (grid-world)
	((obstacles :reader obstacles :initarg :obstacles)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Method: sense
;;; Arguments:
;;; 	- world: the robot-world
;;; 	- agent-meta (key): the agent-meta of the agent
;;; Returns: the senses of the agent
;;; Description:
;;; 	Gets the senses of the agent.
;;; 	Checks whether the agent is blocked.
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 2/24/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod sense ((world robot-world) &key agent-meta)
	(blocked world agent-meta 'forward))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Method: blocked
;;; Arguments:
;;; 	- world: the robot-world
;;; 	- agent-meta: the agent-meta of the agent
;;; 	- direction: the direction to check
;;; Returns: whether the agent is blocked
;;; Description:
;;; 	Checks whether the agent is blocked in the direction.
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 2/24/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod blocked ((world robot-world) agent-meta direction)
	(with-accessors ((rows rows) (columns columns) (agent-metas agent-metas) (obstacles obstacles)) world
		(let* ((coordinate (heading-to-new-coordinate
								(+ (robot-agent-meta-heading agent-meta) (direction-to-heading-adder direction))
								(robot-agent-meta-state agent-meta))))
			(not (not (or
						(member coordinate (mapcar #'cdr agent-metas) :key #'robot-agent-meta-state :test #'equalp)
						(member coordinate obstacles :test #'equalp)
						(member coordinate (borders (1+ rows) (1+ columns) :start -1) :test #'equalp)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Method: act
;;; Arguments:
;;; 	- world: the robot-world
;;; 	- move: a robot-move-action
;;; 	- agent-meta: the agent-meta of the agent
;;; Returns: the current state of agent
;;; Description:
;;; 	Moves the agent according to move action.
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 2/24/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod act ((world robot-world) (move-action robot-move-action) &key agent-meta)
	(with-accessors ((direction robot-move-action-direction)) move-action
		(or (blocked world agent-meta direction)
			(not (with-accessors ((location robot-agent-meta-state) (heading robot-agent-meta-heading)) agent-meta
					(setf location (direction-to-new-coordinate direction heading location)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Method: act
;;; Arguments:
;;; 	- world: the robot-world
;;; 	- turn: a robot-turn-action
;;; 	- agent-meta: the agent-meta of the agent
;;; Returns: the current state of agent
;;; Description:
;;; 	Turns the agent according to turn action.
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 2/24/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod act ((world robot-world) (turn-action robot-turn-action) &key agent-meta)
	(not (with-accessors ((heading robot-agent-meta-heading)) agent-meta
			(setf heading (direction-to-new-heading (robot-turn-action-direction turn-action) heading)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Method: show-
;;; Arguments:
;;; 	- world: the robot-world
;;; 	- coordinate: the coordinate to show
;;; 	- goals (key): the goals to check
;;; Returns: nil
;;; Description:
;;; 	Prints the world in a human-friendly fashion.
;;; 	Prints agent if coordinate is location of an agent.
;;; 	Prints goal if coordinate is location of a goal.
;;; 	Prints obstacle if coordinate is location of an obstacle.
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 2/24/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod show- ((world robot-world) coordinate &key goals)
	(with-accessors ((agent-metas agent-metas) (obstacles obstacles)) world
		(let ((agent-meta (cdr (find coordinate agent-metas :key (lambda (pair) (robot-agent-meta-state (cdr pair))) :test #'equalp))))
			(cond
				(agent-meta (case (robot-agent-meta-heading agent-meta)
								(0 (code-char #x25BA))
								(90 (code-char #x25B2))
								(180 (code-char #x25C4))
								(270 (code-char #x25BC))))
				((member coordinate goals :test #'equalp) (code-char #x25CF))
				((member coordinate obstacles :test #'equalp) (code-char #x25A0))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Method: valid-coordinate-p-
;;; Arguments:
;;; 	- world: the robot-world
;;; 	- coordinate: a coordinate
;;; Returns: whether coordinate is valid
;;; Description:
;;; 	Checks that coordinate is valid.
;;; 	Checks if coordinate is not the location of an agent or obstacle.
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 2/24/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod valid-coordinate-p- ((world robot-world) coordinate)
	(with-accessors ((x coordinate-x) (y coordinate-y)) coordinate
		(and (not (member coordinate (agent-metas world) :key #'cdr :test #'equalp))
			(not (member coordinate (obstacles world) :test #'equalp)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Method: make-world
;;; Arguments:
;;; 	- agent: the agent
;;; 	- rows: the number of rows of world
;;; 	- columns: the number of columns of world
;;; 	- agent-meta: main agent-meta                                 (priority 2)
;;; 	- agent-metas: list of agent-metas                            (priority 1)
;;; 	- p-obstacles: the percentage of obstacles relative to total  (priority 4)
;;; 		number of coordinates
;;; 	- n-obstacles-: the number of obstacles                       (priority 3)
;;; 	- obstacle: the main obstacle                                 (priority 2)
;;; 	- obstacles: the list of obstacles                            (priority 1)
;;; 	- allow-fake-corners: whether to allow fake corners
;;; Returns: the world
;;; Description:
;;; 	Initializes the world.
;;; 	Allows for multiple agents and all combinations of agents and agent-metas.
;;; 	Priorities specify order of importance of arguments if more than one
;;; 	argument of the same group are used.
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 2/24/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-world (agents &key rows columns agent-meta agent-metas (p-obstacles 20) (n-obstacles nil) obstacle obstacles (allow-fake-corners t))
	(make-instance 'robot-world :rows rows
								:columns columns
								:agent-metas (pairlis agents (handle-agent-meta agent-meta agent-metas rows columns (length agents)))
								:obstacles (handle-coordinates (or n-obstacles (random (floor (* (* rows columns) (/ p-obstacles 100)))))
												obstacle
												obstacles
												:filter (when (not allow-fake-corners)
															#'filter-fake-corners))))
