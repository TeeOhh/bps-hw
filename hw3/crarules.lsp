;;;; -*-  Mode: LISP; Syntax: Common-Lisp; Base: 10                          -*-
;;;; ---------------------------------------------------------------------------
;;;; File name: untitled
;;;;    System: 
;;;;    Author: Taylor Olson
;;;;   Created: January 25, 2019 09:06:22
;;;;   Purpose: 
;;;; ---------------------------------------------------------------------------

(in-package :cl-user)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Assign left most number
;; assume carry in column to right
;; check for consistency

;; Sets
;; (assigned M) = letters that are assigned consistent numbers
;; (temp-assigned M) = letters assigned to functions of other variables

;; "Rules"
;; One-to-one. "Two letters are not equal"
;; No letter can be < zero
;; No left most letter of a word can be equal to zero
;; If left-most letter is only number in column, letter = 1

;; GOAL reached when...
;; |assigned| = number of letters. "all letters assigned a number"

;; Predicates
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GreaterThan
;; LessThan
;; EqualTo

(rule ((:true (leftmost-column ?x))
       (:true (column ?x :blank :blank ?y)))
      ;if ?y = 1
      ;(rassert! (assigned ?y))
      ;else, entire set is inconsistent
      )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; End of Code
