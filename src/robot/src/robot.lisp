;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; This file represents the robot main file.
;;;;;
;;;;; Created: 2/24/2019
;;;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Package: robot
;;; Description:
;;; 	Represents the robot package
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 2/24/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defpackage robot
  (:use :cl))

(in-package :robot)

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

(load "robot-agent-meta.lisp")
(load "robot-agent-program.lisp")
(load "robot-agent-sim.lisp")
(load "robot-astar-agent-program.lisp")
(load "robot-bfs-agent-program.lisp")
(load "robot-goal-based-agent-program.lisp")
(load "robot-model-based-agent-program.lisp")
(load "robot-move-action.lisp")
(load "robot-senses.lisp")
(load "robot-simple-reflex-agent-program.lisp")
(load "robot-solution-performance-measure.lisp")
(load "robot-simulator.lisp")
(load "robot-turn-action.lisp")
(load "robot-utility-based-agent-program.lisp")
(load "robot-utils.lisp")
(load "robot-world.lisp")

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
	(run-simulation :agent-program 'robot-simple-reflex-agent-program :rows 25 :columns 25 :n-obstacles 0 :goals (corners 25 25) :show-world-p t)
	(run-simulation :agent-program 'robot-simple-reflex-agent-program :rows 25 :columns 25 :p-obstacles 30 :allow-fake-corners nil :goals (corners 25 25) :show-world-p t)
	(run-simulation :agent-program 'robot-simple-reflex-agent-program :rows 25 :columns 25 :p-obstacles 30 :allow-fake-corners t :goals (corners 25 25) :show-world-p t)

	(run-simulation :agent-program 'robot-model-based-agent-program :rows 25 :columns 25 :n-obstacles 0 :goals (corners 25 25) :show-world-p t)
	(run-simulation :agent-program 'robot-model-based-agent-program :rows 25 :columns 25 :p-obstacles 30 :allow-fake-corners nil :goals (corners 25 25) :show-world-p t)
	(run-simulation :agent-program 'robot-model-based-agent-program :rows 25 :columns 25 :p-obstacles 30 :allow-fake-corners t :goals (corners 25 25) :show-world-p t)

	(run-simulation :agent-program 'robot-goal-based-agent-program :rows 25 :columns 25 :n-obstacles 0 :show-world-p t)
	(run-simulation :agent-program 'robot-goal-based-agent-program :rows 25 :columns 25 :p-obstacles 30 :allow-fake-corners nil :show-world-p t)
	(run-simulation :agent-program 'robot-goal-based-agent-program :rows 25 :columns 25 :p-obstacles 30 :allow-fake-corners t :show-world-p t)

	(run-simulation :agent-program 'robot-bfs-agent-program :rows 25 :columns 25 :n-obstacles 0 :performance-measure 'search-performance-measure :show-world-p t)
	(run-simulation :agent-program 'robot-bfs-agent-program :rows 25 :columns 25 :p-obstacles 30 :allow-fake-corners nil :performance-measure 'search-performance-measure :show-world-p t)
	(run-simulation :agent-program 'robot-bfs-agent-program :rows 25 :columns 25 :p-obstacles 30 :allow-fake-corners t :performance-measure 'search-performance-measure :show-world-p t)

	(run-simulation :agent-program 'robot-a*-agent-program :rows 25 :columns 25 :n-obstacles 0 :performance-measure 'search-performance-measure :show-world-p t)
	(run-simulation :agent-program 'robot-a*-agent-program :rows 25 :columns 25 :p-obstacles 30 :allow-fake-corners nil :performance-measure 'search-performance-measure :show-world-p t)
	(run-simulation :agent-program 'robot-a*-agent-program :rows 25 :columns 25 :p-obstacles 30 :allow-fake-corners t :performance-measure 'search-performance-measure :show-world-p t))

(run-simulator)
