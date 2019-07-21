;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; This file represents the utils functions.
;;;;;
;;;;; Created: 3/2/2019
;;;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Function: curry
;;; Arguments:
;;; 	- function: a function
;;; 	- args (rest): the arguments
;;; Returns: the curried function
;;; Description:
;;; 	Currys a function with the arguments.
;;; 	Taken from https://rosettacode.org/wiki/Currying#Common_Lisp.
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 3/2/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun curry (function &rest args)
	(lambda (&rest more-args) (apply function (append args more-args))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Function: minimum
;;; Arguments:
;;; 	- list: a list
;;; 	- key (key identity): the key of the element
;;; 	- function (key min): a function to get the minimum
;;; Returns: the minimum element of the list
;;; Description:
;;; 	Gets the minimum element of a list.
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 3/2/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun minimum (list &key (key #'identity) (function #'min))
	(find (reduce function list :key key) list :key key))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Function: timings
;;; Arguments:
;;; 	- function: a function
;;; Returns: the runtime of the function
;;; Description:
;;; 	Runs the function and returns its runtime.
;;; 	Taken from https://www.rosettacode.org/wiki/Time_a_function#Common_Lisp.
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 3/2/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun timings (function)
  (let ((real-base (get-internal-real-time))
        (run-base (get-internal-run-time)))
    (funcall function)
    (values (float (/ (- (get-internal-real-time) real-base) internal-time-units-per-second))
            (float (/ (- (get-internal-run-time) run-base) internal-time-units-per-second)))))
