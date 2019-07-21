;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; This file represents the queens-utility-based-agent-program class.
;;;;;
;;;;; Created: 2/25/2019
;;;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Class: queens-utility-based-agent-program
;;; Parent class: utility-based-agent-program
;;; Slots:
;;; 	- test: the function that tests for goal state
;;; 	- adder: the function that gives step cost
;;; 	- heuristic: the function that gives heuristic cost
;;; 	- generator: the function that generates new states
;;; Description:
;;; 	Represents a Queens Utility-based Agent Program.
;;; 	Performs graph search to find path to goal, i.e. where there are n-queens
;;; 	queens and no attacked queens.
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 2/25/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass queens-utility-based-agent-program (utility-based-agent-program)
	((test :reader test :initarg :test)
	(adder :reader adder :initform #'1+)
	(heuristic :reader heuristic :initarg :heuristic)
	(generator :accessor generator :initarg :generator)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Method: initialize-instance
;;; Arguments:
;;; 	- agent-program: a queens-utility-based-agent-program
;;; 	- n-queens (key): the number of queens
;;; Returns: the queens-utility-based-agent-program
;;; Description:
;;; 	Initializes the queens-utility-based-agent-program.
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 2/25/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod initialize-instance :around ((agent-program queens-utility-based-agent-program) &key n-queens)
	(call-next-method agent-program :test (curry #'goal-test n-queens)
									:heuristic (curry #'n-queens-remaining n-queens)
									:generator (curry #'unattacked-positions n-queens)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Method: action--
;;; Arguments:
;;; 	- agent-program: a queens-utility-based-agent-program
;;; 	- queens: the queens
;;; Returns: an action
;;; Description:
;;; 	Makes an action from the queens.
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 2/25/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod action-- ((agent-program queens-utility-based-agent-program) queens)
	(make-queens-place-action :position (car (last queens))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Method: graph
;;; Arguments:
;;; 	- agent-program: a queens-utility-based-agent-program
;;; Returns: a graph
;;; Description:
;;; 	Makes a graph to search.
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 2/24/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod graph ((agent-program queens-utility-based-agent-program))
	(with-accessors ((test test) (agent-meta agent-meta) (adder adder) (heuristic heuristic) (generator generator)) agent-program
		(with-accessors ((queens agent-meta-state)) agent-meta
			(let ((node (make-instance 'node :label queens :adder adder :heuristic heuristic :generator generator)))
				(make-instance 'graph :root node :test test)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Function: goal-test
;;; Arguments:
;;; 	- n-queens: the number of queens
;;; 	- queens: the queens
;;; Returns: whether the queens represent the goal state
;;; Description:
;;; 	Checks if there are n-queens queens and no queens are attacked.
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 2/24/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun goal-test (n-queens queens)
	(and (= (length queens) n-queens) (= (n-attacked-queens n-queens queens) 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Function: n-queens-remaining
;;; Arguments:
;;; 	- n-queens: the number of queens
;;; 	- queens: the queens
;;; Returns: the number of queens remaining to be placed
;;; Description:
;;; 	Gets the number of queens remaining to be placed.
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 2/24/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun n-queens-remaining (n-queens queens)
	(- n-queens (length queens)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Function: n-attacked-queens
;;; Arguments:
;;; 	- n-queens: the number of queens
;;; 	- queens: the queens
;;; Returns: the number of attacked queens
;;; Description:
;;; 	Gets the number of attacked queens in queens.
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 2/24/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun n-attacked-queens (n-queens queens)
	(loop for queen in queens
		count (attacked-position-p n-queens (remove queen queens) queen)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Function: unattacked-positions
;;; Arguments:
;;; 	- n-queens: the number of queens
;;; 	- queens: the queens
;;; Returns: the new states where a queen can safely be placed
;;; Description:
;;; 	Gets the next states where the new queen is unattacked.
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 2/24/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun unattacked-positions (n-queens queens)
	(let* ((queen (car (last queens)))
			(column (if queen
						(1+ (coordinate-x queen))
						0)))
		(when (< column n-queens)
			(loop for row from 0 below n-queens
					for queen = (make-coordinate :x column :y row)
				when (when (not (attacked-position-p n-queens queens queen))
						(append queens (list queen)))
				collect it))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Function: attacked-position-p
;;; Arguments:
;;; 	- n-queens: the number of queens
;;; 	- queens: the queens
;;; 	- position: the position
;;; Returns: whether the position is attacked
;;; Description:
;;; 	Checks whether the position is under attack by any queens.
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 2/24/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun attacked-position-p (n-queens queens position)
	(let ((positions (positions n-queens position)))
		(some (lambda (queen) (member queen positions :test #'equalp)) queens)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Function: positions
;;; Arguments:
;;; 	- n-queens: the number of queens
;;; 	- position: the position
;;; Returns: a list of coordinates
;;; Description:
;;; 	Gets a list of all possible positions after a move made by a queen at
;;; 	position.
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 2/24/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun positions (n-queens position)
	(append (list position) (horizontal-positions n-queens position) (vertical-positions n-queens position) (diagonal-positions n-queens position)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Function: horizontal-positions
;;; Arguments:
;;; 	- n-queens: the number of queens
;;; 	- position: the position
;;; Returns: a list of coordinates
;;; Description:
;;; 	Gets a list of all possible positions after a horizontal move made by a
;;; 	queen at position.
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 2/24/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun horizontal-positions (n-queens position)
	(loop for column from 0 below n-queens
		collect (make-coordinate :x column :y (coordinate-y position))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Function: vertical-positions
;;; Arguments:
;;; 	- n-queens: the number of queens
;;; 	- position: the position
;;; Returns: a list of coordinates
;;; Description:
;;; 	Gets a list of all possible positions after a vertical move made by a
;;; 	queen at position.
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 2/24/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun vertical-positions (n-queens position)
	(loop for row from 0 below n-queens
		collect (make-coordinate :x (coordinate-x position) :y row)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Function: diagonal-positions
;;; Arguments:
;;; 	- n-queens: the number of queens
;;; 	- position: the position
;;; Returns: a list of coordinates
;;; Description:
;;; 	Gets a list of all possible positions after a diagonal move made by a
;;; 	queen at position.
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 2/24/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun diagonal-positions (n-queens position)
	(append
		(loop for row from (coordinate-y position) below n-queens
			for column from (coordinate-x position) below n-queens
			while (and (< row n-queens) (< column n-queens))
		collect (make-coordinate :x column :y row))
		
		(loop for row downfrom (coordinate-y position) to 0
			for column from (coordinate-x position) below n-queens
			while (and (< row n-queens) (< column n-queens))
		collect (make-coordinate :x column :y row))
		
		(loop for row downfrom (coordinate-y position) to 0
			for column downfrom (coordinate-x position) to 0
			while (and (< row n-queens) (< column n-queens))
		collect (make-coordinate :x column :y row))
		
		(loop for row from (coordinate-y position) below n-queens
			for column downfrom (coordinate-x position) to 0
			while (and (< row n-queens) (< column n-queens))
		collect (make-coordinate :x column :y row))))
