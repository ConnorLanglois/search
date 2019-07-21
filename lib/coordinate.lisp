;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; This file represents the coordinate structure.
;;;;;
;;;;; Created: 2/24/2019
;;;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setf *random-state* (make-random-state t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Structure: coordinate
;;; Parent structure: none
;;; Slots:
;;; 	- x: the x dimension
;;; 	- y: the y dimension
;;; Description:
;;; 	Represents coordinate/point in space.
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 2/24/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct coordinate
	(x 0 :read-only t)
	(y 0 :read-only t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Function: handle-coordinates
;;; Arguments:
;;; 	- n-coordinates: the number of coordinates to randomly make if coordinate not given
;;; 	- coordinate: a coordinate pair to be duplicated
;;; 	- coordinates: a list of coordinate pairs
;;; 	- n-nested-coordinates (key): the number of list of coordinates to be nested
;;; 	- nested-coordinates (key): a list of list of coordinate pairs
;;; 	- same-coordinates (key): duplicate coordinates if t, otherwise randomly make
;;; 	- filter (key): a function to filter out coordinates
;;; Returns: a list of coordinates
;;; Description:
;;; 	Handles many different types of inputs and creates or duplicates and
;;; 	converts coordinate pairs (e.g. (5 6), representing x = 5 and y = 6)
;;; 	into coordinates.
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 2/24/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun handle-coordinates (n-coordinates coordinate coordinates &key n-nested-coordinates nested-coordinates same-coordinates filter)
	(if (or coordinate coordinates nested-coordinates)
		(let ((coordinates (list-to-coordinates (or coordinates (list coordinate)) :nested-lists nested-coordinates)))
			(if (and n-nested-coordinates (not nested-coordinates))
				(nest n-nested-coordinates (lambda () coordinates))
				coordinates))
		(if n-nested-coordinates
			(if same-coordinates
				(make-list n-nested-coordinates :initial-element (make-random-coordinates n-coordinates :filter filter))
				(nest n-nested-coordinates (lambda () (make-random-coordinates n-coordinates :filter filter))))
			(make-random-coordinates n-coordinates :filter filter))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Function: list-to-coordinates
;;; Arguments:
;;; 	- lists: a list of coordinate pairs or list of list of coordinate pairs,
;;; 		depending on value of nested-lists
;;; 	- nested-lists (key): convert list of list of coordinate pairs if t,
;;; 		otherwise convert list of coordinate pairs.
;;; Returns: a list of list of coordinates or list of coordinates
;;; Description:
;;; 	Converts a list of list of coordinate pairs or a list of coordinate pairs
;;; 	to coordinates.
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 2/24/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun list-to-coordinates (lists &key nested-lists)
	(if nested-lists
		(mapcar #'list-to-coordinates nested-lists)
		(mapcar #'list-to-coordinate lists)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Function: list-to-coordinate
;;; Arguments:
;;; 	- list: a coordinate pair
;;; Returns: a coordinate
;;; Description:
;;; 	Converts a coordinate pair to coordinates.
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 2/24/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun list-to-coordinate (list)
	(let ((coordinate (make-coordinate :x (first list) :y (second list))))
		(remove-coordinate coordinate)
		coordinate))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Function: coordinate+
;;; Arguments:
;;; 	- coordinate1: a coordinate
;;; 	- coordinate2: a coordinate
;;; Returns: a coordinate
;;; Description:
;;; 	Adds two coordinates.
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 2/24/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun coordinate+ (coordinate1 coordinate2)
	(make-coordinate :x (+ (coordinate-x coordinate1) (coordinate-x coordinate2)) :y (+ (coordinate-y coordinate1) (coordinate-y coordinate2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Function: coordinate-
;;; Arguments:
;;; 	- coordinate1: a coordinate
;;; 	- coordinate2: a coordinate
;;; Returns: a coordinate
;;; Description:
;;; 	Substracts two coordinates.
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 2/24/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun coordinate- (coordinate1 coordinate2)
	(make-coordinate :x (- (coordinate-x coordinate1) (coordinate-x coordinate2)) :y (- (coordinate-y coordinate1) (coordinate-y coordinate2))))

(let (rows columns coordinates current-coordinates store-current)
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;;; Function: make-random-coordinates
	;;; Arguments:
	;;; 	- n: the number of random coordinates to make
	;;; 	- filter (key): a function to filter out coordinates
	;;; Returns: a list of coordinates
	;;; Description:
	;;; 	Makes a list of random coordinates.
	;;; Author: Connor Langlois <connor.langlois@maine.edu>
	;;; Created: 2/24/2019
	;;; Modifications: none
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	(defun make-random-coordinates (n &key filter)
		(let (coordinates)
			(setf store-current t)
			(setf coordinates (loop for m from 1 to n
				collect (make-random-coordinate :filter filter)))
			(setf current-coordinates nil)
			(setf store-current nil)
			coordinates))

	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;;; Function: make-random-coordinate
	;;; Arguments:
	;;; 	- filter (key): a function to filter out coordinates
	;;; Returns: a coordinate
	;;; Description:
	;;; 	Makes a random coordinate.
	;;; Author: Connor Langlois <connor.langlois@maine.edu>
	;;; Created: 2/24/2019
	;;; Modifications: none
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	(defun make-random-coordinate (&key filter)
		(let ((coordinate (choose (if filter
									(funcall filter rows columns coordinates current-coordinates)
									coordinates))))
			(setf coordinates (remove coordinate coordinates))
			(when store-current
				(setf current-coordinates (append current-coordinates (list coordinate))))
			coordinate))
	
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;;; Function: set-bounds
	;;; Arguments:
	;;; 	- rows-: the new number of rows
	;;; 	- columns-: the new number of columns
	;;; Returns: the columns
	;;; Description:
	;;; 	Set bounds for coordinate generation.
	;;; Author: Connor Langlois <connor.langlois@maine.edu>
	;;; Created: 2/24/2019
	;;; Modifications: none
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	(defun set-bounds (rows- columns-)
		(setf rows rows- columns columns-))
	
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;;; Function: set-coordinates
	;;; Arguments:
	;;; 	- coordinate-: a list of coordinates
	;;; Returns: the coordinates
	;;; Description:
	;;; 	Sets coordinates to be used for coordinate generation/picking.
	;;; Author: Connor Langlois <connor.langlois@maine.edu>
	;;; Created: 2/24/2019
	;;; Modifications: none
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	(defun set-coordinates (coordinates-)
		(setf coordinates coordinates-))
	
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;;; Function: remove-coordinates
	;;; Arguments:
	;;; 	- coordinate-: a coordinate
	;;; Returns: the resulting coordinates
	;;; Description:
	;;; 	Removes coordinate from coordinates.
	;;; Author: Connor Langlois <connor.langlois@maine.edu>
	;;; Created: 2/24/2019
	;;; Modifications: none
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	(defun remove-coordinate (coordinate-)
		(setf coordinates (remove coordinate- coordinates :test #'equalp))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Function: generate-coordinates
;;; Arguments:
;;; 	- rows: the number of rows
;;; 	- columns: the number of columns
;;; Returns: a list of coordinates
;;; Description:
;;; 	Generates a grid of coordinates.
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 2/24/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun generate-coordinates (rows columns)
	(loop for row from 0 below rows
		append (loop for column from 0 below columns
					collect (make-coordinate :x column :y row))))
