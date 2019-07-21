;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; This file represents the agent class.
;;;;;
;;;;; Created: 2/25/2019
;;;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Class: agent
;;; Parent class: none
;;; Slots:
;;; 	- agent-program: the agent-program
;;; Description:
;;; 	Represents the agent.
;;; 	Receives percepts and produces action.
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 2/25/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass agent ()
	((agent-program :reader agent-program :initarg :agent-program)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Method: sensors
;;; Arguments:
;;; 	- agent: an agent
;;; 	- senses (optional nil): the senses to be passed to the agent-program
;;; Returns: the percepts of agent-program
;;; Description:
;;; 	Passes senses to the agent-program
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 2/24/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod sensors ((agent agent) &optional (senses nil))
	(percept (agent-program agent) senses))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Method: action
;;; Arguments:
;;; 	- agent: an agent
;;; Returns: an action of agent-program
;;; Description:
;;; 	Receives action from agent-program
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 2/24/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod action ((agent agent))
	(action (agent-program agent)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Function: make-agents
;;; Arguments:
;;; 	- agents: a list of agent symbols
;;; 	- agent-programs: a list of agent-program symbols
;;; Returns: a list of agents
;;; Description:
;;; 	Makes agents with agent-programs
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 2/24/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-agents (agents agent-programs)
	(mapcar #'make-agent agents agent-programs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Function: make-agent
;;; Arguments:
;;; 	- agent: an agent symbol
;;; 	- agent-program: a agent-program symbol
;;; Returns: an agent
;;; Description:
;;; 	Makes agent with agent-program
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 2/24/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-agent (agent agent-program)
	(make-instance agent :agent-program agent-program))
