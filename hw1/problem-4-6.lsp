;;;; -*-  Mode: LISP; Syntax: Common-Lisp; Base: 10                          -*-
;;;; ---------------------------------------------------------------------------
;;;; File name: problem-4-6.lsp
;;;;    System: 
;;;;    Author: Taylor Olson
;;;;   Created: January 13, 2019 12:53:47
;;;;   Purpose: 
;;;; ---------------------------------------------------------------------------
;;;;  $LastChangedDate: 2018-09-27 12:08:59 -0500 (Thu, 27 Sep 2018) $
;;;;  $LastChangedBy: usher $
;;;; ---------------------------------------------------------------------------

(in-package :cl-user)

;Assumes a does not depend on b
;(there doesn't exist a rule that show b, gives us a)
;Example rule: show cloudy sky gives us blue sky
;and fetching (show (and (blue sky) (cloudy Sky))
;we don't have blue sky explicitly so it will fail before checking cloudy
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(rule (show (and ?a ?b))
      (assert! `(show ,?a))
      (rule ?a
            (assert! `(show ,?b))
            (rule ?b (assert! `(and ,?a ,?b)))))

(rule (show (iff ?a ?b))
      (assert! `(show (implies ,?a ,?b)))
      (rule (implies ?a ?b)
            (assert! `(show (implies ,?b ,?a)))
            (rule (implies ?b ?a)
                  (assert! `(iff ,?a ,?b)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; End of Code
