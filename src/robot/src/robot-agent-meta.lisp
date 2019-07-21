;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; This file represents the robot-agent-meta structure.
;;;;;
;;;;; Created: 2/24/2019
;;;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Structure: robot-agent-meta
;;; Parent structure: agent-meta
;;; Slots:
;;; 	- heading: the heading of the agent
;;; Description:
;;; 	Represents agent from point of view of world and agent.
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 2/24/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (robot-agent-meta (:include agent-meta))
	(heading 90))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Method: handle-agent-meta
;;; Arguments:
;;; 	- agent-meta: an agent-meta
;;; 	- agent-metas: a list of agent-meta
;;; 	- rows: the number of rows of the world
;;; 	- columns: the number of columns of the world
;;; 	- n-agents: the number of agents to make if agent-meta not given
;;; Returns: the new generator
;;; Description:
;;; 	Stores the new world and generator.
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 2/24/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun handle-agent-meta (agent-meta agent-metas rows columns n-agents)
	(if (or agent-meta agent-metas)
		(list-to-agent-metas (or agent-metas (make-list n-agents :initial-element agent-meta)))
		(make-random-agent-metas rows columns n-agents)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Function: list-to-agent-metas
;;; Arguments:
;;; 	- lists: a list of list of agent-meta data
;;; Returns: a list of agent-meta
;;; Description:
;;; 	Converts list of list of agent-meta data to list of agent-meta.
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 2/24/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun list-to-agent-metas (lists)
	(mapcar #'list-to-agent-meta lists))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Function: list-to-agent-meta
;;; Arguments:
;;; 	- list: a list of agent-meta data
;;; Returns: an agent-meta
;;; Description:
;;; 	Converts a list of agent-meta data to agent-meta.
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 2/24/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun list-to-agent-meta (list)
	(make-robot-agent-meta :state (make-coordinate :x (first list) :y (second list)) :heading (third list)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Function: make-random-agent-metas
;;; Arguments:
;;; 	- rows: the number of rows of the world
;;; 	- columns: the number of columns of the world
;;; 	- n-agents: the number of random agent-meta to make
;;; Returns: a list of agent-meta
;;; Description:
;;; 	Makes a list of random agent-metas.
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 2/24/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-random-agent-metas (rows columns n-agents)
	(loop for n from 1 to n-agents
		collect (make-random-agent-meta rows columns)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Function: make-random-agent-meta
;;; Arguments:
;;; 	- rows: the number of rows of the world
;;; 	- columns: the number of columns of the world
;;; Returns: an agent-meta
;;; Description:
;;; 	Makes a random agent-meta.
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 2/24/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-random-agent-meta (rows columns)
	(make-robot-agent-meta :state (make-random-coordinate) :heading (random-heading)))
