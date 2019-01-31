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

;; nothing can be less than zero
(rule ((:true (value-of ?z 0))
       (:true (column ?col ?a ?b ?c)))
      (rassert! (:not (less-than ?a ?z)))
      (rassert! (:not (less-than ?b ?z)))
      (rassert! (:not (less-than ?c ?z))))

;; if we know a and b, then assert c
(rule ((:true (value-of ?a ?a-val))
       (:true (value-of ?b ?b-val))
       (:true (column ?col ?a ?b ?c))
       (:true (carry-from (1- ?col))))
      (if (> (+ ?a ?b) 10)
        ;assert carry-from
        (rassert! (value-of ?c (- (+ ?a ?b) 9)))
        ;assert :false carry-from
        (rassert! (value-of ?c (1+ (+ ?a ?b))))))

;; if we know a and b, then assert c
(rule ((:true (value-of ?a ?a-val))
       (:true (value-of ?b ?b-val))
       (:true (column ?col ?a ?b ?c))
       (:false (carry-from (1- ?col))))
      (if (> (+ ?a ?b) 10)
        ;assert carry-from
        (rassert! (value-of ?c (- (+ ?a ?b) 10)))
        ;assert :false carry-from
        (rassert! (value-of ?c (+ ?a ?b)))))
                   
;; if both a and b blank then c must be equal to 1, if a blank then c = b + 1, same for b blank
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

;; if c = b - 1, the top or bot must be 8 (if carry in right column) and 9 if not
(rule ((:true (column ?col ?a ?b ?c))
       (:true (value-of ?a ?a-val))
       (:true (value-of ?c ?c-val)))
      (if (= ?c-val (1- ?a-val))
        (assert! `(:or (:and (value-of ,?b 8) (carry-from ,(1- ?col)))
                       (value-of ,?b 9)) 'one-less)))

(rule ((:true (column ?col ?a ?b ?c))
       (:true (value-of ?b ?b-val))
       (:true (value-of ?c ?c-val)))
      (if (= ?c-val (1- ?b-val))
        (assert! `(:or (:and (value-of ,?a 8) (carry-from ,(1- ?col)))
                       (value-of ,?a 9)) 'one-less)))

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
      (:not (carry-from `(1- ,?col)))
      (:true (column ?col ?a ?b ?c)))
      (rassert! (sum-to ?c (- (+ ?a ?b) 10)))
      (rassert! (sum-to ?b (- (+ ?c 10) ?a-val)))
      (rassert! (sum-to ?a (- (+ ?c 10) ?b-val))))

;; if there's a carry from current column and from previous, then x + y - 1 (mod 10) =  z
(rule ((:true (carry-from ?col))
      (:true (carry-from `(1- ,?col)))
      (:true (column ?col ?a ?b ?c)))
      (rassert! (sum-to ?c (- (+ ?a ?b) 9)))
      (rassert! (sum-to ?b (- (+ ?c 9) ?a)))
      (rassert! (sum-to ?a (- (+ ?c 9) ?b))))

;; if there's a carry in a column, then the result is less than the top and bottom
(rule ((:true (carry-from ?col))
       (:true (column ?col ?a ?b ?c)))
      (rassert! (less-than ?c ?a))
      (rassert! (less-than ?c ?b)))

;; if a top or bottom is zero, then there can't be a carry from the column and c = 1 + (a or b)
(rule ((:true (column ?col ?a ?b ?c))
       (:true (value-of ?a 0)))
      (assert! `(carry-from ,(1- ?col)) 'zero-in-column)
      (rassert! (sum-to ?c (1+ ?b-))))

(rule ((:true (column ?col ?a ?b ?c))
       (:true (value-of ?b 0)))
      (assert! `(carry-from ,(1- ?col)) 'zero-in-column)
      (rassert! (sum-to ?c (1+ ?a))))

;; if we know the algebraic relationship between a b and c and know 2 of em, then we know 3rd
(rule ((:true (value-of ?b ?b-val))
       (:true (value-of ?c ?c-val))
       (:true (column ?col ?a ?b ?c))
       (:true (sum-to ?a ?sum)))
      (setq ?b ?b-val)
      (setq ?c ?c-val)
      (rassert! (value-of ?a (eval sum))))

(rule ((:true (value-of ?a ?a-val))
       (:true (value-of ?c ?c-val))
       (:true (column ?col ?a ?b ?c))
       (:true (sum-to ?b ?sum)))
      (setq ?a ?a-val)
      (setq ?c ?c-val)
      (rassert! (value-of ?b (eval sum))))

(rule ((:true (value-of ?a ?a-val))
       (:true (value-of ?b ?b-val))
       (:true (column ?col ?a ?b ?c))
       (:true (sum-to ?c ?sum)))
      (setq ?a ?a-val)
      (setq ?b ?b-val)
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
