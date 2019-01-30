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
;; Sets
;; (assigned M) = letters that are assigned consistent numbers
;; (temp-assigned M) = letters assigned to functions of other variables

;; GOAL reached when...
;; |assigned| = number of letters. "all letters assigned a number"

;; No 2 numbers can be equal
(rule ((:true (value-of ?x ?valx))
       (:true (value-of ?y ?valy)))
      (if (eql ?valx ?valy)
          (rassert! (:not ?valx)))) ; Choose whichever one is assumed

;; If left-most letter is only number in column, letter = 1
(rule ((:true (leftmost-column ?x))
       (:true (column ?x :blank :blank ?y)))
      (rassert! (value-of ?y 1))
      (rassert! (carry-in ?x))) ; find how to do 1- ?x here

;; Left most letters cannot be zero


;; If carry in col, result is less than top and bottom and sum of top and bottom > 10
(rule ((:true (carry-in ?x))
       (:true (column ?x ?a ?b ?c)))
      (rassert! (more-than-ten ?a ?b))
      (rassert! (less-than ?c ?a))
      (rassert! (less-than ?c ?b)))

(rule ((:true (less-than ?x ?y))
       (:true (value-of ?x ?valx))
       (:true (value-of ?y ?valy)))
      (if (>= ?valx ?valy)
          (rassert! (:not (value-of ?x ?valx))))) ;How do I know which one is not consistent? Whichever one is assumed.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; End of Code
