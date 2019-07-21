;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; This file represents the utils functions.
;;;;;
;;;;; Created: 2/24/2019
;;;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Function: degrees-to-radians
;;; Arguments:
;;; 	- degrees: the number of degrees
;;; Returns: the radians
;;; Description:
;;; 	Converts degrees to radians.
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 2/24/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun degrees-to-radians (degrees)
	(* degrees (/ pi 180)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Function: radians-to-degrees
;;; Arguments:
;;; 	- radians: the number of radians
;;; Returns: the degrees
;;; Description:
;;; 	Converts radians to degrees.
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 2/24/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun radians-to-degrees (radians)
	(* radians (/ 180 pi)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Function: nest
;;; Arguments:
;;; 	- n: the number of nested elements
;;; 	- fun: a function to invoke
;;; Returns: the list of elements
;;; Description:
;;; 	Invokes function n times and collects the results.
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 2/24/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun nest (n fun)
	(loop for n from 1 to n
		collect (funcall fun)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Function: choose
;;; Arguments:
;;; 	- list: a list of elements
;;; Returns: a random element
;;; Description:
;;; 	Chooses a random element of the list.
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 2/24/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun choose (list)
	(nth (random (length list)) list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Function: random-heading
;;; Arguments: none
;;; Returns: a random heading
;;; Description:
;;; 	Chooses a random heading (0, 90, 180, 270).
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 2/24/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun random-heading ()
	(choose '(0 90 180 270)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Function: heading-to-coordinate
;;; Arguments:
;;; 	- heading: a heading
;;; Returns: a coordinate
;;; Description:
;;; 	Converts a heading to a coordinate.
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 2/24/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun heading-to-coordinate (heading)
	(let ((radians (degrees-to-radians heading)))
		(make-coordinate :x (truncate (cos radians)) :y (truncate (sin radians)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Function: heading-to-new-coordinate
;;; Arguments:
;;; 	- heading: a heading
;;; 	- coordinate: a coordinate
;;; Returns: a new coordinate
;;; Description:
;;; 	Converts a heading and coordinate to a new coordinate.
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 2/24/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun heading-to-new-coordinate (heading coordinate)
	(coordinate+ coordinate (heading-to-coordinate heading)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Function: cardinal-to-direction
;;; Arguments:
;;; 	- cardinal: a cardinal direction
;;; 	- heading: a heading
;;; Returns: a direction
;;; Description:
;;; 	Converts a cardinal to a direction.
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 2/24/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun cardinal-to-direction (cardinal heading)
	(when cardinal
		(heading-to-direction (mod (- cardinal heading) 360))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Function: heading-to-direction
;;; Arguments:
;;; 	- heading: a heading
;;; Returns: a direction
;;; Description:
;;; 	Converts a heading to a direction.
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 2/24/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun heading-to-direction (heading)
	(cdr (assoc heading '((0 . forward) (90 . left) (270 . right) (180 . backward)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Function: bearing-to-direction
;;; Arguments:
;;; 	- bearing: a bearing
;;; 	- heading: a heading
;;; Returns: a coordinate
;;; Description:
;;; 	Converts a bearing to a direction.
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 2/24/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun bearing-to-direction (bearing heading)
	(cardinal-to-direction (bearing-to-cardinal bearing) heading))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Function: bearing-to-cardinal
;;; Arguments:
;;; 	- bearing: a bearing
;;; Returns: a cardinal
;;; Description:
;;; 	Converts a bearing to a cardinal.
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 2/24/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun bearing-to-cardinal (bearing)
	(cond
		((or (and (>= bearing 315) (< bearing 360)) (and (>= bearing 0) (< bearing 45))) 0)
		((and (>= bearing 45) (< bearing 135)) 90)
		((and (>= bearing 135) (< bearing 225)) 180)
		((and (>= bearing 225) (< bearing 315)) 270)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Function: bearing
;;; Arguments:
;;; 	- coordinate1: a coordinate
;;; 	- coordinate2: a coordinate
;;; Returns: a bearing
;;; Description:
;;; 	Calculates bearing between two coordinates.
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 2/24/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun bearing (coordinate1 coordinate2)
	(let ((coordinate (coordinate- coordinate2 coordinate1)))
		(with-accessors ((x coordinate-x) (y coordinate-y)) coordinate
			(cond
				((and (= x 0) (= y 0)) nil)
				((and (= x 0) (> y 0)) 90)
				((and (= x 0) (< y 0)) 270)
				(t (let ((arc (radians-to-degrees (atan (/ y x)))))
					(mod (if (< x 0)
							(+ arc 180)
							arc) 360)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Function: direction-to-heading-adder
;;; Arguments:
;;; 	- direction: a direction
;;; Returns: a number
;;; Description:
;;; 	Converts a direction to a number.
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 2/24/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun direction-to-heading-adder (direction)
	(cdr (assoc direction '((forward . 0) (left . 90) (right . 270) (backward . 180)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Function: direction-to-new-heading
;;; Arguments:
;;; 	- direction: a direction
;;; 	- heading: a heading
;;; Returns: a heading
;;; Description:
;;; 	Converts a direction to a heading.
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 2/24/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun direction-to-new-heading (direction heading)
	(mod (+ heading
			(direction-to-heading-adder direction))
		360))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Function: direction-to-new-coordinate
;;; Arguments:
;;; 	- direction: a direction
;;; 	- heading: a heading
;;; 	- coordinate: a coordinate
;;; Returns: a new coordinate
;;; Description:
;;; 	Converts a direction, heading, and coordinate to a new coordinate.
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 2/24/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun direction-to-new-coordinate (direction heading coordinate)
	(heading-to-new-coordinate (direction-to-new-heading direction heading) coordinate))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Function: direction-to-part
;;; Arguments:
;;; 	- direction: a direction
;;; Returns: a part
;;; Description:
;;; 	Converts a direction to a part (front-bump, left-bump, right-bump,
;;; 	rear-bump).
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 2/24/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun direction-to-part (direction)
	(cdr (assoc direction '((forward . :front-bump) (left . :left-bump) (right . :right-bump) (backward . :rear-bump)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Function: heading-to-coordinate
;;; Arguments:
;;; 	- heading: a heading
;;; Returns: a coordinate
;;; Description:
;;; 	Converts a heading to a coordinate.
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 2/24/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun part-to-direction (part)
	(cdr (assoc part '((:front-bump . forward) (:left-bump . left) (:right-bump . right) (:rear-bump . backward)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Function: filter-fake-corners
;;; Arguments:
;;; 	- rows: the rows of the world
;;; 	- columns: the columns of the world
;;; 	- coordinates: a list of coordinates
;;; 	- obstacles: a list of obstacles
;;; Returns: a list of coordinates
;;; Description:
;;; 	Filters out fake corners from coordinates.
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 2/24/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun filter-fake-corners (rows columns coordinates obstacles)
	(let ((fake-corners (fake-corners rows columns coordinates obstacles)))
		(remove-if (lambda (coordinate) (member coordinate fake-corners :test #'equalp)) coordinates)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Function: fake-corners
;;; Arguments:
;;; 	- rows: the rows of the world
;;; 	- columns: the columns of the world
;;; 	- coordinates: a list of coordinates
;;; 	- obstacles: a list of obstacles
;;; Returns: a list of coordinates
;;; Description:
;;; 	Gets the fake corners of the obstacles.
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 2/24/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun fake-corners (rows columns coordinates obstacles)
	(append
		(borders rows columns)
		(loop for obstacle in obstacles
			append (with-slots (x y) obstacle
						(list (make-coordinate :x (1- x) :y (1+ y))
							(make-coordinate :x (1+ x) :y (1+ y))
							(make-coordinate :x (1- x) :y (1- y))
							(make-coordinate :x (1+ x) :y (1- y)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Function: borders
;;; Arguments:
;;; 	- rows: the rows of the world
;;; 	- columns: the columns of the world
;;; 	- start (key 0): the dimension at which to start
;;; Returns: a list of coordinates
;;; Description:
;;; 	Gets the borders with rows and columns.
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 2/24/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun borders (rows columns &key (start 0))
	(remove nil (loop for row from start below rows
					append (loop for column from start below columns
								collect (cond
											((= row start) (make-coordinate :x column :y start))
											((= row (1- rows)) (make-coordinate :x column :y (1- rows)))
											((= column start) (make-coordinate :x start :y row))
											((= column (1- columns)) (make-coordinate :x (1- columns) :y row)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Function: corners
;;; Arguments:
;;; 	- rows: the rows of the world
;;; 	- columns: the columns of the world
;;; Returns: a list of coordinates
;;; Description:
;;; 	Gets the corners with rows and columns.
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 2/24/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun corners (rows columns)
	(list (list 0 0)
		(list 0 (1- rows))
		(list (1- columns) 0)
		(list (1- columns) (1- rows))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Function: neighbors
;;; Arguments:
;;; 	- coordinate: a coordinate
;;; Returns: a list of coordinates
;;; Description:
;;; 	Gets the neighbor coordinates of coordinate.
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 2/24/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun neighbors (coordinate)
	(with-accessors ((x coordinate-x) (y coordinate-y)) coordinate
		(list
			(make-coordinate :x x :y (1+ y))
			(make-coordinate :x (1- x) :y y)
			(make-coordinate :x (1+ x) :y y)
			(make-coordinate :x x :y (1- y)))))
