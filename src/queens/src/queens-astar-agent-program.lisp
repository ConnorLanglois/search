;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; This file represents the queens-a*-agent-program class.
;;;;;
;;;;; Created: 2/25/2019
;;;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Class: queens-a*-agent-program
;;; Parent class: queens-utility-based-agent-program, a*-agent-program
;;; Slots: none
;;; Description:
;;; 	Represents a Queens Utility-based Agent Program.
;;; 	Represents a A* Agent Program.
;;; Author: Connor Langlois <connor.langlois@maine.edu>
;;; Created: 2/25/2019
;;; Modifications: none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass queens-a*-agent-program (queens-utility-based-agent-program a*-agent-program) ())
