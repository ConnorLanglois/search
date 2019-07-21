;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; This file represents the queens-world class.
;;;;;
;;;;; Created: 2/25/2019
;;;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Class: queens-world
;;; Parent class: grid-world
;;; Slots: none
;;; Description:
;;; 	Represents the grid-based world.
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 2/25/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass queens-world (grid-world) ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Method: act
;;; Arguments:
;;; 	- world: the queens-world
;;; 	- place-action: a queens-place-action
;;; Returns: the current state of agent
;;; Description:
;;; 	Places a queen according to place action.
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 2/25/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod act ((world queens-world) (place-action queens-place-action) &key &allow-other-keys)
	(with-accessors ((queen queens-place-action-position)) place-action
		(with-accessors ((queens agent-meta-state)) (agent-meta world)
			(setf queens (append queens (list queen))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Method: show-
;;; Arguments:
;;; 	- world: the queens-world
;;; 	- position: a position (i.e. coordinate)
;;; Returns: "black queen" unicode character if position member of queens,
;;; 	otherwise nil
;;; Description:
;;; 	Acts out the action.
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 2/25/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod show- ((world queens-world) position)
	(with-accessors ((queens agent-meta-state)) (agent-meta world)
		(when (member position queens :test #'equalp)
			(code-char #x265B))))
