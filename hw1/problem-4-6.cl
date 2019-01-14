;;;; -*-  Mode: LISP; Syntax: Common-Lisp; Base: 10                          -*-
;;;; ---------------------------------------------------------------------------
;;;; File name: problem-4-6
;;;;    System: 
;;;;    Author: Taylor Olson
;;;;   Created: January 13, 2019 12:53:47
;;;;   Purpose: 
;;;; ---------------------------------------------------------------------------
;;;;  $LastChangedDate: 2018-09-27 12:08:59 -0500 (Thu, 27 Sep 2018) $
;;;;  $LastChangedBy: usher $
;;;; ---------------------------------------------------------------------------

(in-package :cl-user)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; They both assume that ?a and ?b being equal should result in True?
(rule (show (and ?a ?b))
      (assert! '(show ,?a))
      (assert! '(show ,?b))
      ;stop if ?a not found
      (rule ?a (rule ?b (assert! '(and ,?b)))))

(rule (show (iff ?a ?b))
      (assert! '(show (implies ,?a ,?b)))
      (assert! '(show (implies ,?b ,?a)))
      ;stop if (implies ?a ?b) not found
      ;(and (implies ?a ?b) (implies ?b ?a))
      (rule (implies ?a ?b)
            (rule (implies ?b ?a)
                  (assert! '(iff ,?a ,?b)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; End of Code
