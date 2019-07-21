;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; This file represents the simulator class.
;;;;;
;;;;; Created: 3/2/2019
;;;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Class: simulator
;;; Parent class: none
;;; Slots:
;;; 	- world: the world
;;; 	- agent: the agent (if only one)
;;; 	- agent-sim: the agent-sim (if only one)
;;; 	- agent-sims: the agent-sims
;;; Description:
;;; 	Represents the main simulator.
;;; 	Contains main loop of program, runs agents, world, and performance
;;; 	measures.
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 3/2/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass simulator ()
	((world :reader world :initarg :world)
	(agent :reader agent :initarg :agent)
	(agent-sim :reader agent-sim :initarg :agent-sim)
	(agent-sims :reader agent-sims :initarg :agent-sims)
	(show-world-p :reader show-world-p :initform t :initarg :show-world-p)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Method: initialize-instance
;;; Arguments:
;;; 	- simulator: a simulator
;;; 	- rest (rest): the rest of slots
;;; 	- agent (key): the agent slot
;;; 	- agent-sim (key): the agent-sim slot
;;; Returns: the simulator
;;; Description:
;;; 	Initializes the simulator.
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 3/2/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod initialize-instance :around ((simulator simulator) &rest rest &key agent agent-sim)
	(apply #'call-next-method simulator (append (when (and agent agent-sim)
													(list :agent-sims (list (cons agent agent-sim)))) rest)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Method: simulate
;;; Arguments:
;;; 	- simulator: a simulator
;;; Returns: nil
;;; Description:
;;; 	The main loop of the program.
;;; 	Runs through each agent, gets senses from world, gives agent senses,
;;; 	gets action from agent, and gives action to world.
;;; 	Continues until agent state has not changed for 10 iterations.
;;; 	Runs performance measures of agent at end.
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 3/2/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod simulate ((simulator simulator))
	(performance simulator (timings (curry #'-simulate simulator))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Method: -simulate
;;; Arguments:
;;; 	- simulator: a simulator
;;; Returns: nil
;;; Description:
;;; 	The main loop of the program.
;;; 	Runs through each agent, gets senses from world, gives agent senses,
;;; 	gets action from agent, and gives action to world.
;;; 	Continues until agent state has not changed for 10 iterations.
;;; 	Runs performance measures of agent at end.
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 3/2/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod -simulate ((simulator simulator))
	(with-accessors ((world world) (agent-sims agent-sims)) simulator
		(let* ((agent-counters (pairlis (mapcar #'car agent-sims) (make-list (length agent-sims) :initial-element 0))))
			(when (show-world-p simulator)
				(loop for agent-counter in agent-counters
					do (show-- simulator (agent-sim- simulator (car agent-counter)))))
			(loop until (every (lambda (counter) (= counter 10)) (mapcar #'cdr agent-counters))
				do (loop for agent-counter in agent-counters
						if (< (cdr agent-counter) 10)
							do (let* ((agent (car agent-counter))
										(agent-sim (agent-sim- simulator agent))
										(agent-meta (agent-meta- world agent))
										(state (agent-meta-state agent-meta)))
									(simulate- simulator agent agent-sim agent-meta)
									(with-accessors ((path agent-sim-path)) agent-sim
										(with-accessors ((new-state agent-meta-state)) agent-meta
											(if (not (equalp new-state state))
												(progn
													(setf path (append path (list new-state)))
													(when (show-world-p simulator)
														(show-- simulator agent-sim))
													(setf (cdr agent-counter) 0))
												(incf (cdr agent-counter)))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Method: performance
;;; Arguments:
;;; 	- simulator: a simulator
;;; Returns: nil
;;; Description:
;;; 	Runs performance measures on agent.
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 3/2/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod performance ((simulator simulator) runtime)
	(loop for agent in (agent-sims simulator)
			do  (with-accessors ((agent-program agent-program)) (car agent)
					(with-accessors ((test agent-sim-test) (path agent-sim-path) (performance-measures agent-sim-performance-measures)) (cdr agent)
						(if (funcall test (car (last path)))
							(format t "~%~a Succeeded~%" (type-of agent-program))
							(format t "~%~a Failed~%" (type-of agent-program)))
						(with-accessors ((n-expanded n-expanded) (max-size max-size)) agent-program
							(loop for performance-measure in performance-measures
								do (if (typep performance-measure 'search-performance-measure)
										(measure performance-measure n-expanded max-size)
										(performance- simulator (cdr agent) performance-measure))
									(format t "Runtime: ~a~%" runtime)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Method: show--
;;; Arguments:
;;; 	- simulator: a simulator
;;; 	- agent-sim: the agent-sim
;;; Returns: nil
;;; Description:
;;; 	Shows the world.
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 3/2/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod show-- ((simulator simulator) agent-sim)
	(show (world simulator)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Method: agent-sim-
;;; Arguments:
;;; 	- simulator: a simulator
;;; 	- agent: an agent
;;; Returns: an agent-sim
;;; Description:
;;; 	Gets agent-sim associated with agent.
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 3/2/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod agent-sim- ((simulator simulator) agent)
	(cdr (assoc agent (agent-sims simulator))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Generic function: simulate-
;;; Arguments:
;;; 	- simulator: a simulator
;;; 	- agent: an agent
;;; 	- agent-sim: the agent-sim of agent
;;; 	- agent-meta: the agent-meta of agent
;;; Returns: the current state of agent
;;; Description:
;;; 	Shows the world, gets senses, gets action from agent, and gives action
;;; 	to world.
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 3/2/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric simulate- (simulator agent agent-sim agent-meta))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Generic function: performance-
;;; Arguments:
;;; 	- simulator: a simulator
;;; 	- agent-sim: the agent-sim of agent
;;; 	- performance-measure: a performance-measure of the agent
;;; Returns: nil
;;; Description:
;;; 	Runs performance measure on agent.
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 3/2/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric performance- (simulator agent-sim performance-measure))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Function: run-simulation
;;; Arguments:
;;; 	- simulator: a simulator symbol
;;; 	- rest (rest): the arguments of the simulator
;;; Returns: nil
;;; Description:
;;; 	Makes the simulator with the arguments and simulates.
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 3/2/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun run-simulation- (simulator &rest rest)
	(simulate (apply #'make-instance simulator rest)))
