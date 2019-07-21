;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; This file represents the grid-world class.
;;;;;
;;;;; Created: 3/2/2019
;;;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Class: grid-world
;;; Parent class: world
;;; Slots:
;;; 	- rows: the number of rows
;;; 	- columns: the number of columns
;;; Description:
;;; 	Represents a grid-based world.
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 3/2/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass grid-world (world)
	((rows :reader rows :initarg :rows)
	(columns :reader columns :initarg :columns)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Method: show
;;; Arguments:
;;; 	- world: a world
;;; 	- rest (rest): arguments to be passed to show-
;;; Returns: nil
;;; Description:
;;; 	Prints the world in a human-friendly fashion.
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 3/2/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod show ((world world) &rest rest &key &allow-other-keys)
	(with-accessors ((rows rows) (columns columns)) world
		(loop for row from (1- rows) downto 0
			do (format t "~v@{~a~:*~}" columns "+---") (format t "+~%")
				(loop for column from 0 below columns
					do (format t "| ~a "
							(let* ((coordinate (make-coordinate :x column :y row)))
								(or (apply #'show- world coordinate rest) " "))))
				(format t "|")
				(format t "~%"))
		(format t "~v@{~a~:*~}" columns "+---") (format t "+~%~%")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Method: valid-coordinate-p
;;; Arguments:
;;; 	- world: a world
;;; 	- coordinate: a coordinate
;;; Returns: whether coordinate is valid
;;; Description:
;;; 	Checks that coordinate is inside world bounds.
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 3/2/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod valid-coordinate-p ((world world) coordinate)
	(with-accessors ((x coordinate-x) (y coordinate-y)) coordinate
		(and (>= x 0) (< x (columns world)) (>= y 0) (< y (rows world))
			(valid-coordinate-p- world coordinate))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Method: valid-coordinate-p-
;;; Arguments:
;;; 	- world: a world
;;; 	- coordinate: a coordinate
;;; Returns: whether coordinate is valid
;;; Description:
;;; 	Checks that coordinate is valid.
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 3/2/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod valid-coordinate-p- ((world world) coordinate)
	t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Method: world-neighbors
;;; Arguments:
;;; 	- world: a world
;;; 	- coordinate: a coordinate
;;; Returns: a list of coordinates
;;; Description:
;;; 	Gets the valid neighbor coordinates of coordinate.
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 3/2/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod world-neighbors ((world world) coordinate)
	(let ((neighbors (neighbors coordinate)))
		(remove-if-not (lambda (neighbor) (valid-coordinate-p world neighbor)) neighbors)))
