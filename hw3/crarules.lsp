;;;; -*-  Mode: LISP; Syntax: Common-Lisp; Base: 10                          -*-
;;;; ---------------------------------------------------------------------------
;;;; File name: crarules.lsp
;;;;    System: 
;;;;    Author: Taylor Olson
;;;;   Created: January 25, 2019 09:06:22
;;;;   Purpose: 
;;;; ---------------------------------------------------------------------------

(in-package :cl-user)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GOAL reached when...
;; |value-of| = number of letters. "all letters assigned a number"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Predicates
;; (less-than ?x ?y)
;; (non-zero ?x)
;; (carry-from ?col) 1- col for previous and col for current
;; (sum-to ?a ?b ?c)

;; keep track of those numbers already assigned
(rule ((:true (value-of ?a ?a-val)))
      (rassert! (assigned ?a-val)))

;; if both a and b blank then c must be equal to 1, if a blank then c = (a or b) + 1
(rule ((:true (column ?col ?a ?b ?c)))
      (cond ((and (eql ?a :blank) (eql ?b :blank))
             (rassert! (value-of ?c 1))
             (assert! `(carry-from ,(1- ?col)) 'carry-in-leftof-column))
            ((eql ?a :blank)
             (rassert! (one-more-than ?c ?b))
             (assert! `(carry-from ,(1- ?col)) 'carry-in-leftof-column))
            ((eql ?b :blank)
             (rassert! (one-more-than ?c ?a))
             (assert! `(carry-from ,(1- ?col)) 'carry-in-leftof-column))))

;; if we know a is one more than b and a is known, assert b = a - 1
(rule ((:true (one-more-than ?a ?b))
       (:true (value-of ?a ?val)))
      (rassert! (value-of ?b `(1- ,?val))))
;; same, but we know b...
(rule ((:true (one-more-than ?a ?b))
       (:true (value-of ?b ?val)))
      (rassert! (value-of ?b `(1+ ,?val))))

;; if we know (less-than a b) and a is known, assert :false for all b <= a
(rule ((:true (less-than ?a ?b))
       (:true (value-of ?a ?val)))
      ;; check what is taken here
      (let* ((assigned (mapcan 'cdr (fetch '(assigned ?x))))
            (potential (loop for i from (1+ ?val) to 9 collect i))
             (unassigned (set-difference potential assigned)))
        (if (= (length unassigned) 1)
          (assert! `(value-of ,?b ,(car unassigned)) 'elimination)
          (loop for ?i from 0 to ?val
                do (rassert! (:not (value-of ?b ?i)))))))

;; same, but we know a...
(rule ((:true (less-than ?a ?b))
       (:true (value-of ?b ?val)))
      ;; check what is taken here
      (let* ((assigned (mapcan 'cdr (fetch '(assigned ?x))))
            (potential (loop for i from 0 to (1- ?val) collect i))
             (unassigned (set-difference potential assigned)))
        (if (= (length unassigned) 1)
          (assert! `(value-of ,?a ,(car unassigned)) 'elimination)
          (loop for ?i from ?val to 9
                do (rassert! (:not (value-of ?a ?i)))))))

;; if there's a carry from current column and non from previous, then x + y (mod 10) = z
(rule ((:true (carry-from ?col))
      (:false (carry-from `(1- ,?col)))
      (:true (column ?col ?a ?b ?c)))
      (rassert! (sum-to ?c (- (+ ?a-val ?b-val) 10)))
      (rassert! (sum-to ?b (- (+ ?c-val 10) ?a-val)))
      (rassert! (sum-to ?a (- (+ ?c-val 10) ?b-val))))

;; if there's a carry from current column and from previous, then x + y - 1 (mod 10) =  z
(rule ((:true (carry-from ?col))
      (:true (carry-from `(1- ,?col)))
      (:true (column ?col ?a ?b ?c)))
      (rassert! (sum-to ?c (- (+ ?a-val ?b-val) 9)))
      (rassert! (sum-to ?b (- (+ ?c-val 9) ?a-val)))
      (rassert! (sum-to ?a (- (+ ?c-val 9) ?b-val))))

;; if there's a carry in a column, then the result is less than the top and bottom
(rule ((:true (carry-from ?col))
       (:true (column ?col ?a ?b ?c)))
      (rassert! (less-than ?c ?a))
      (rassert! (less-than ?c ?b)))

;; if a top or bottom is zero, then there can't be a carry from the column and c = 1 + (a or b)
(rule ((:true (column ?col ?a ?b ?c))
       (:true (value-of ?a 0)))
      (assert! `(carry-from ,(1- ?col)) 'zero-in-column)
      (rassert! (sum-to ?c (1+ ?b))))

(rule ((:true (column ?col ?a ?b ?c))
       (:true (value-of ?b 0)))
      (assert! `(carry-from ,(1- ?col)) 'zero-in-column)
      (rassert! (sum-to ?c (1+ ?a))))

;; if we know the algebraic relationship between a b and c and know 2 of em, then we know 3rd
(rule ((:true (column ?col ?a ?b ?c))
       (:true (value-of ?b ?b-val))
       (:true (value-of ?c ?c-val))
       (:true (sum-to ?a ?sum)))
      (rassert! (value-of ?a (eval sum))))

(rule ((:true (column ?col ?a ?b ?c))
       (:true (value-of ?a ?a-val))
       (:true (value-of ?c ?c-val))
       (:true (sum-to ?b ?sum)))
      (rassert! (value-of ?b (eval sum))))

(rule ((:true (column ?col ?a ?b ?c))
       (:true (value-of ?a ?a-val))
       (:true (value-of ?b ?b-val))
       (:true (sum-to ?c ?sum)))
      (rassert! (value-of ?c (eval sum))))

;; Left most letters cannot be zero
(rule ((:true (leftmost-column ?col))
       (:true (column ?col ?a ?b ?c)))
      (assert-non-zero ?col ?a)
      (assert-non-zero ?col ?b)
      (assert-non-zero ?col ?c))

(defun assert-non-zero (col var)
  (do* ((a-tracker var (caddar (fetch `(column ,col-tracker ?a ?b ?c))))
           (col-tracker col (1- col-tracker)))
          ((not (eql a-tracker ':blank)) (assert! `(:not (value-of ,a-tracker 0)) 'left-most))))







;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; End of Code
