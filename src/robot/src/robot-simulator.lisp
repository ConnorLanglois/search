;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; This file represents the robot-simulator class.
;;;;;
;;;;; Created: 2/24/2019
;;;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Class: robot-simulator
;;; Parent class: simulator
;;; Slots: none
;;; Description:
;;; 	Represents the main simulator.
;;; 	Contains main loop of program, runs agents, world, and performance
;;; 	measures.
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 2/24/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass robot-simulator (simulator) ())

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
;;; Created: 2/24/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod simulate- ((simulator robot-simulator) agent agent-sim agent-meta)
	(with-accessors ((world world)) simulator
		(let (action)
			(with-accessors ((prior-bump robot-agent-sim-prior-bump)) agent-sim
				(sensors agent (apply #'make-robot-senses :front-sensor (sense world :agent-meta agent-meta) prior-bump))
				(setf action (action agent))
				(setf prior-bump nil)
				(if (typep action 'robot-move-action)
					(setf prior-bump (list (direction-to-part (robot-move-action-direction action)) (act world action :agent-meta agent-meta)))
					(act world action :agent-meta agent-meta))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Method: performance-
;;; Arguments:
;;; 	- simulator: a simulator
;;; 	- agent-sim: the agent-sim of agent
;;; 	- performance-measure: a performance-measure of the agent
;;; Returns: nil
;;; Description:
;;; 	Runs solution performance measure on agent.
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 2/24/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod performance- ((simulator robot-simulator) (agent-sim robot-agent-sim) (performance-measure robot-solution-performance-measure))
	(measure performance-measure (agent-sim-path agent-sim) (world simulator) (robot-agent-sim-goals agent-sim)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Method: show--
;;; Arguments:
;;; 	- simulator: a queens-simulator
;;; 	- agent-sim: the agent-sim
;;; Returns: nil
;;; Description:
;;; 	Shows the world.
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 2/24/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod show-- ((simulator robot-simulator) agent-sim)
	(show (world simulator) :goals (robot-agent-sim-goals agent-sim)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Function: run-simulation
;;; Arguments:
;;; 	- rest (rest): the arguments of the simulator
;;; Returns: nil
;;; Description:
;;; 	Makes the simulator with the arguments and simulates.
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 2/24/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun run-simulation (&rest rest)
	(simulate (apply #'make-simulator rest)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Method: make-simulator
;;; Arguments:
;;; 	- rest (rest): the other arguments to be passed to world
;;; 	- rows (key): the number of rows of world
;;; 	- columns (key): the number of columns of world
;;; 	- agent (key): the main agent                                 (priority 2)
;;; 	- agents (key): the list of agents                            (priority 1)
;;; 	- agent-program (key): the main agent-program                 (priority 2)
;;; 	- agent-programs (key): the list of agent-programs            (priority 1)
;;; 	- performance-measure (key): the main performance-measure     (priority 3)
;;; 	- performance-measures- (key): the list of
;;; 		performance-measures, duplicated for each agent           (priority 2)
;;; 	- performance-measures (key): the list of list of
;;; 		performance-measures (one list for each agent)            (priority 1)
;;; 	- n-goals (key): the number of goals                          (priority 4)
;;; 	- goal (key): the main goal, duplicated for each agent        (priority 3)
;;; 	- goals- (key): the list of goals, duplicated for each agent  (priority 2)
;;; 	- goals (key): the list of list of goals (one list for        (priority 1)
;;; 		each agent)
;;; 	- same-goal (key): if all agents should use same goal
;;; 		(if priority 4)
;;; 	- same-goals (key): if all agents should use same goals
;;; 		(if priority 4)
;;; 	- show-world-p (key): whether to show the world
;;; Returns: the simulator
;;; Description:
;;; 	Initializes the simulator.
;;; 	Really massive scaffolding function to create the simulator.
;;; 	Sorry for that..
;;; 	Allows for multiple agents and all combinations of agents, agent-programs,
;;; 	performance-measures, and goals.
;;; 	Priorities specify order of importance of arguments if more than one
;;; 	argument of the same group are used.
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 2/25/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-simulator (&rest rest
						&key
							(rows 25)
							(columns 25)
							(agent 'agent)
							agents
							agent-program
							agent-programs
							(performance-measure 'robot-solution-performance-measure)
							performance-measures-
							performance-measures
							(n-goals 1)
							goal
							goals-
							goals
							(same-goal nil)
							(same-goals nil)
							(show-world-p t)
						&allow-other-keys)
	;; make main simulator
	(let (world agent-sims)
		(set-bounds rows columns)
		(set-coordinates (generate-coordinates rows columns)) ; generates all possible coordinates with row and columns and sets them as the coordinates to use for random coordinates
		(setf agent-programs (or agent-programs (list agent-program)))
		(setf goals (handle-coordinates n-goals goal (if (= (length agent-programs) 1) ; given n-goals, goal, or list of goals (goals- or goals), create goals
														(or goals- goals)
														goals-) :n-nested-coordinates (length agent-programs) :nested-coordinates (if (/= (length agent-programs) 1)
																																		goals) :same-coordinates (or same-goal same-goals)))
		(setf agent-programs (make-agent-programs agent-programs goals)) ; makes agent-programs with agent-programs and goals
		(setf agents (make-agents (or agents (make-list (length agent-programs) :initial-element agent)) agent-programs)) ; make agents with agent-programs, duplicating agent if list not given
		(setf world (apply #'make-world agents :rows rows :columns columns :allow-other-keys t rest)) ; make the world with agents, rows, columns, and any other keys passed in (e.g. obstacles, allowing fake corners, ...) 
		(setf performance-measures (make-performance-measures (or
																performance-measures
																(when performance-measures- (make-list (length agent-programs) :initial-element performance-measures-))
																(make-list (length agent-programs) :initial-element (list performance-measure))))) ; make performance-measures, duplicating performance-measure if list not given
		(setf agent-sims (pairlis agents (make-agent-sims :goals goals :locations (mapcar #'robot-agent-meta-state	 (mapcar #'cdr (agent-metas world))) :performance-measures performance-measures))) ; reassign goals as an alist of agents and goals

		;; give the world to all utility-based-agent-programs
		(loop for agent in agents
			do (with-accessors ((agent-program agent-program)) agent
					(cond
						((typep agent-program 'utility-based-agent-program) (setf (world agent-program) world (agent-meta agent-program) (agent-meta- world agent)))
						((typep agent-program 'goal-based-agent-program) (setf (agent-meta agent-program) (agent-meta- world agent))))))

		;; create main simulator instance with world, agent-sims, performance-measures, and goals
		(make-instance 'robot-simulator :world world
										:agent-sims agent-sims
										:show-world-p show-world-p)))
