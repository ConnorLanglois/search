;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; This file represents the queens main file.
;;;;;
;;;;; Created: 2/24/2019
;;;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Package: queens
;;; Description:
;;; 	Represents the queens package
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 2/24/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defpackage queens
  (:use :cl))

(in-package :queens)

(setf sb-impl::*default-external-format* :UTF-8)

(load "../../../lib/action.lisp")
(load "../../../lib/agent.lisp")
(load "../../../lib/agent-meta.lisp")
(load "../../../lib/agent-program.lisp")
(load "../../../lib/agent-sim.lisp")
(load "../../../lib/astar-agent-program.lisp")
(load "../../../lib/bfs-agent-program.lisp")
(load "../../../lib/coordinate.lisp")
(load "../../../lib/graph.lisp")
(load "../../../lib/goal-based-agent-program.lisp")
(load "../../../lib/grid-world.lisp")
(load "../../../lib/model-based-agent-program.lisp")
(load "../../../lib/node.lisp")
(load "../../../lib/nop-action.lisp")
(load "../../../lib/performance-measure.lisp")
(load "../../../lib/search-performance-measure.lisp")
(load "../../../lib/senses.lisp")
(load "../../../lib/simulator.lisp")
(load "../../../lib/simple-reflex-agent-program.lisp")
(load "../../../lib/utility-based-agent-program.lisp")
(load "../../../lib/utils.lisp")
(load "../../../lib/world.lisp")

(load "queens-astar-agent-program.lisp")
(load "queens-bfs-agent-program.lisp")
(load "queens-place-action.lisp")
(load "queens-simulator.lisp")
(load "queens-utility-based-agent-program.lisp")
(load "queens-world.lisp")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Function: run-simulator
;;; Arguments: none
;;; Returns: nil
;;; Description:
;;; 	Runs the simulations.
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 2/24/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun run-simulator ()
	;; (run-simulation :agent-program 'queens-bfs-agent-program :n-queens 7 :show-world-p t)
	;; (run-simulation :agent-program 'queens-bfs-agent-program :n-queens 8 :show-world-p t)
	;; (run-simulation :agent-program 'queens-bfs-agent-program :n-queens 9 :show-world-p t)

	;; (run-simulation :agent-program 'queens-a*-agent-program :n-queens 7 :show-world-p t)
	(run-simulation :agent-program 'queens-a*-agent-program :n-queens 8 :show-world-p t))
	;; (run-simulation :agent-program 'queens-a*-agent-program :n-queens 16 :show-world-p t))

(run-simulator)
