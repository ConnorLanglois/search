;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; This file represents the queens-simulator class.
;;;;;
;;;;; Created: 2/25/2019
;;;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Class: queens-simulator
;;; Parent class: simulator
;;; Slots: none
;;; Description:
;;; 	Represents the main simulator.
;;; 	Contains main loop of program, runs agents, world, and performance
;;; 	measures.
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 2/25/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass queens-simulator (simulator) ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Method: initialize-instance
;;; Arguments:
;;; 	- simulator: a simulator
;;; 	- n-queens (key 8): the number of queens (aka number of rows and columns)
;;; 	- agent (key agent): an agent symbol
;;; 	- agent-program (key): an agent-program symbol
;;; 	- performance-measure (key search-performance-measure): a performance measure symbol
;;; Returns: the simulator
;;; Description:
;;; 	Initializes the simulator.
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 2/25/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod initialize-instance :around ((simulator queens-simulator) &key
																		(n-queens 8)
																		(agent 'agent)
																		agent-program
																		(performance-measure 'search-performance-measure)
																		(show-world-p t))
	(let* ((agent-program (make-instance agent-program :n-queens n-queens))
			(agent (make-instance agent :agent-program agent-program))
			(agent-meta (make-agent-meta))
			(world (make-instance 'queens-world :rows n-queens :columns n-queens :agent agent :agent-meta agent-meta))
			(performance-measure (make-instance performance-measure))
			(agent-sim (make-agent-sim :test (curry #'goal-test n-queens) :performance-measures (list performance-measure))))
		(setf (world agent-program) world)
		(setf (agent-meta agent-program) agent-meta)
		(call-next-method simulator :world world :agent agent :agent-sim agent-sim :show-world-p show-world-p)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Method: simulate-
;;; Arguments:
;;; 	- simulator: a queens-simulator
;;; 	- agent: the agent
;;; 	- agent-sim: the agent-sim
;;; 	- agent-meta: the agent-meta
;;; Returns: nil
;;; Description:
;;; 	Shows world, gets senses from world, gives agent senses,
;;; 	gets action from agent, and gives action to world.
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 2/25/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod simulate- ((simulator queens-simulator) agent agent-sim agent-meta)
	(with-accessors ((world world)) simulator
		(sensors agent)
		(act world (action agent))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Function: run-simulation
;;; Arguments:
;;; 	- rest (rest): the arguments of the simulator
;;; Returns: nil
;;; Description:
;;; 	Makes the simulator with the arguments and simulates.
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 2/25/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun run-simulation (&rest rest)
	(apply #'run-simulation- 'queens-simulator rest))
