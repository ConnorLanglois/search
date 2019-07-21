;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; This file represents the world class.
;;;;;
;;;;; Created: 3/2/2019
;;;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Class: world
;;; Parent class: none
;;; Slots:
;;; 	- agent: the agent (if only one)
;;; 	- agent-meta: the agent-meta (if only one)
;;; 	- agent-metas: the agent-metas
;;; Description:
;;; 	Represents the world.
;;; 	Contains agents and agent-metas.
;;; 	Senses agent sensors and acts agent actions.
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 3/2/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass world ()
	((agent :reader agent :initarg :agent)
	(agent-meta :reader agent-meta :initarg :agent-meta)
	(agent-metas :accessor agent-metas :initarg :agent-metas)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Method: initialize-instance
;;; Arguments:
;;; 	- world: a world
;;; 	- rest (rest): the rest of slots
;;; 	- agent (key): the agent slot
;;; 	- agent-meta (key): the agent-meta slot
;;; Returns: the world
;;; Description:
;;; 	Initializes the world.
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 3/2/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod initialize-instance :around ((world world) &rest rest &key agent agent-meta)
	(apply #'call-next-method world (append (when (and agent agent-meta)
												(list :agent-metas (list (cons agent agent-meta)))) rest)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Method: act
;;; Arguments:
;;; 	- world: the world
;;; 	- action: a nop action
;;; 	- agent-meta (key): the agent-meta of the agent
;;; Returns: the current state of agent
;;; Description:
;;; 	Acts out the action.
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 3/2/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod act (world (action nop-action) &key agent-meta))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Method: agent-meta-
;;; Arguments:
;;; 	- world: the world
;;; 	- agent: an agent
;;; Returns: an agent-meta
;;; Description:
;;; 	Gets agent-meta associated with agent
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 3/2/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod agent-meta- ((world world) agent)
	(cdr (assoc agent (agent-metas world))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Generic function: sense
;;; Arguments:
;;; 	- world: the world
;;; 	- agent-meta (key): the agent-meta of the agent
;;; Returns: the senses of the agent
;;; Description:
;;; 	Gets the senses of the agent.
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 3/2/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric sense (world &key agent-meta))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Generic function: act
;;; Arguments:
;;; 	- world: the world
;;; 	- action: an action
;;; 	- agent-meta (key): the agent-meta of the agent
;;; Returns: the current state of agent
;;; Description:
;;; 	Acts out the action.
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 3/2/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric act (world action &key agent-meta))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Generic function: show
;;; Arguments:
;;; 	- world: the world
;;; 	- rest (rest): the rest of arguments to show
;;; Returns: nil
;;; Description:
;;; 	Prints the world in a human-friendly fashion.
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 3/2/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric show (world &rest rest &key &allow-other-keys))
