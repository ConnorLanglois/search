;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; This file represents the graph class.
;;;;;
;;;;; Created: 2/24/2019
;;;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Class: graph
;;; Parent class: none
;;; Slots:
;;; 	- root: the root node
;;; 	- test: a function that tests for goal state
;;; Description:
;;; 	Represents a graph.
;;; 	Has Breadth-First-Search and A* search methods.
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 2/24/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass graph ()
	((root :reader root :initarg :root)
	(test :reader test :initarg :test)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Method: bfs
;;; Arguments:
;;; 	- graph: a graph
;;; Returns: a path to goal, number of nodes expanded, and max size of frontier
;;; Description:
;;; 	Performs Breadth-First-Search on the graph.
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 2/24/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod bfs ((graph graph))
	(with-accessors ((root root) (test test)) graph
		(when (funcall test (label root))
			(return-from bfs (values (list (label root)) 0 0)))
		(let ((frontier (list root)) (explored '()) (n-expanded 0) (max-size 0))
			(loop while (> (length frontier) 0)
				do (setf max-size (max max-size (length frontier)))
					(incf n-expanded)
					(let ((node (pop frontier)))
						(with-accessors ((label label)) node
							(if explored
								(push label (cdr (last explored)))
								(push label explored))
							(loop for successor in (successors node)
									if (not (member (label successor) (append explored (mapcar #'label frontier)) :test #'equalp))
								do (if (funcall test (label successor))
										(return-from bfs (values (mapcar #'label (path successor)) n-expanded max-size))
										(if frontier
											(push successor (cdr (last frontier)))
											(push successor frontier)))))))
			(values nil n-expanded max-size))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Method: a*
;;; Arguments:
;;; 	- graph: a graph
;;; Returns: a path to goal, number of nodes expanded, and max size of frontier
;;; Description:
;;; 	Performs A* on the graph.
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 2/24/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod a* ((graph graph))
	(with-accessors ((root root) (test test)) graph
		(let ((frontier (list root)) (explored '()) (n-expanded 0) (max-size 0))
			(loop while (> (length frontier) 0)
				do (setf max-size (max max-size (length frontier)))
					(incf n-expanded)
					(let ((node (minimum frontier :function #'min-node)))
						(setf frontier (remove node frontier))
						(with-accessors ((label label)) node
							(when (funcall test label)
								(return-from a* (values (mapcar #'label (path node)) n-expanded max-size)))
							(if explored
								(push label (cdr (last explored)))
								(push label explored))
							(loop for successor in (successors node)
									if (not (member (label successor) explored :test #'equalp))
								do (let ((old-successor (find successor frontier :test #'node=)))
									(cond 
										((not old-successor) (if frontier
																(push successor (cdr (last frontier)))
																(push successor frontier)))
										((< (cost successor) (cost old-successor))
											(setf frontier (substitute successor old-successor frontier :test #'node=)))))))))
			(values nil n-expanded max-size))))
