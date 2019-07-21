;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; This file represents the performance-measure class.
;;;;;
;;;;; Created: 2/24/2019
;;;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Class: performance-measure
;;; Parent class: none
;;; Slots: none
;;; Description:
;;; 	Represents a performance measure.
;;; 	Measures the performance of an agent.
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 2/24/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass performance-measure () ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Generic function: measure
;;; Arguments:
;;; 	- performance-measure: a performance-measure
;;; 	- args (rest): the arguments
;;; Returns: nil
;;; Description:
;;; 	Measures the performance of the agent.
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 2/24/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric measure (performance-measure &rest args))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Function: make-performance-measures
;;; Arguments:
;;; 	- performance-measures: a list of performance-measure symbols
;;; Returns: a list of performance-measures
;;; Description:
;;; 	Converts list of performance-measure symbols to list of
;;; 	performance-measures.
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 2/24/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-performance-measures (performance-measures)
	(mapcar #'make-performance-measures- performance-measures))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Function: make-performance-measure
;;; Arguments:
;;; 	- performance-measure: a performance-measure symbol
;;; Returns: a performance-measure
;;; Description:
;;; 	Converts performance-measure symbol to performance-measure.
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 2/24/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-performance-measures- (performance-measures-)
	(mapcar #'make-instance performance-measures-))
