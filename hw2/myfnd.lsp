;;;; -*-  Mode: LISP; Syntax: Common-Lisp; Base: 10                          -*-
;;;; ---------------------------------------------------------------------------
;;;; File name: myfnd.lsp
;;;;    System: 
;;;;    Author: Taylor Olson
;;;;   Created: January 19, 2019 12:46:11
;;;;   Purpose: 
;;;; ---------------------------------------------------------------------------
;;;;  $LastChangedDate: 2018-09-27 12:08:59 -0500 (Thu, 27 Sep 2018) $
;;;;  $LastChangedBy: usher $
;;;; ---------------------------------------------------------------------------

(in-package :cl-user)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; premise and goal rules

;true if x is an assumption of problem
(rule ((premise ?x))
      ;maybe (rassert! (premise ?x))
      (rassert! ?x))

;true if x's proof is goal of problem
(rule ((goal ?x))
      (rassert! (show ?x))
      (rule ((goal ?y) ?y)
            (rassert! (goal ?x))))

(defun solved? (ftre problem)
  (run-forms ftre problem)
  ;will this be true no matter what since (goal something) is asserted in problem statement?
  (if (fetch '(goal ?x) "Problem Solved")))

(defun transform-statement (problem)
  (mapcar #'(lambda (assertion)
             (cond ((eq (caadar (cdr assertion)) 'show)
                    (cons (car assertion) (list (cons 'goal (cdr (cadadr assertion))))))
                   (t (cons (car assertion) (list (cons 'premise (cdadr assertion)))))))
          problem))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; First, some utilities:

(defvar *debug-nd* nil)

(defmacro debug-nd (format-string &rest args)
  `(if *debug-nd*
       (format t ,format-string ,@ args)))

(defvar *connective-list* '(implies and or iff not))

(defun simple-proposition? (x)
  (or (not (listp x))
      (not (member (car x) *connective-list*))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Implementing the KM* natural deduction system
;; We begin by Modus ponens (conditional elimination)
;; and its friends.

(rule ((implies ?p ?q) ?p) ;; conditional elimination
      (debug-nd "~%~D: CE: ~A" (ftre-depth *ftre*) ?q)
      (rassert! ?q))

(rule ((show ?q) ;; backward chaining.
       :test (not (fetch ?q))
       (implies ?p ?q))
      (rassert! (show ?p)))

(a-rule ((show (implies ?p ?q)) ;; Conditional Introduction
	 :test (not (fetch `(implies ,?p ,?q))))
    (debug-nd "~%~D: Trying CI on (implies ~A ~A)."
	      (ftre-depth *ftre*) ?p ?q)
    (when (seek-in-context ?p `(or ,?q contradiction))
      (debug-nd "~%~D: CI: (implies ~A ~A)"
		(ftre-depth *ftre*) ?p ?q)
      (rassert! (implies ?p ?q))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; And elimination and introduction

(rule ((and . ?conjuncts)) ;AND elimination
      (dolist (conjunct ?conjuncts)
	(debug-nd "~%~D: AE: ~A" (ftre-depth *ftre*) conjunct)
	(assert! conjunct)))

(rule ((show (and ?c1 ?c2)))
      (rassert! (show ?c1))
      (rassert! (show ?c2))
      (rule (?c1 ?c2) (rassert! (and ?c1 ?c2))))

(rule ((show (and ?c1 ?c2 ?c3)))
      (rassert! (show ?c1))
      (rassert! (show ?c2))
      (rassert! (show ?c3))
      (rule (?c1 ?c2 ?c3) (rassert! (and ?c1 ?c2 ?c3))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Biconditional elimination and introduction

(rule ((iff ?p ?q)) ;; IFF elimination
    (debug-nd "~%~D: BE: ~A~%~D: BE: ~A"
      (ftre-depth *ftre*) `(implies ,?p ,?q)
      (ftre-depth *ftre*) `(implies ,?q ,?p))
    (rassert! (implies ?p ?q))
    (rassert! (implies ?q ?p)))

(rule ((show (iff ?p ?q)) ;IFF introduction
       :test (not (fetch `(iff ,?p ,?q))))
    (debug-nd "~%~D: BI-BC: (show (implies ~A ~A))"
			   (ftre-depth *ftre*) ?p ?q)
    (debug-nd "~%~D: BI-BC: (show (implies ~A ~A))" 
	      (ftre-depth *ftre*) ?q ?p)
    (rassert! (show (implies ?p ?q)))
    (rassert! (show (implies ?q ?p)))
    (rule ((implies ?p ?q) (implies ?q ?p))
	  (debug-nd "~%~D: BI: ~A"
		    (ftre-depth *ftre*) `(iff ,?p ,?q))
	  (rassert! (iff ?p ?q))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Dealing with negation

(rule ((not (not ?p)))
      (debug-nd "~%~D: NE: ~A" (ftre-depth *ftre*) ?p)
      (rassert! ?p)) ;; NOT elimination

(a-rule ((show (not ?p)) ;; NOT introduction
	 :test (not (or (fetch `(not ,?p))
			(eq ?p 'contradiction))))
    (debug-nd "~%~D: NI attempt: (not ~A)"
	      (ftre-depth *ftre*) ?p)
    (when (seek-in-context ?p 'contradiction)
      (debug-nd "~%~D: NI: ~A" (ftre-depth *ftre*) `(not ,?p))
      (rassert! (not ?p))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Disjunction elimination and introduction

(rule ((show (or . ?disjuncts))) ;; OR introduction
      (dolist (?disjunct ?disjuncts)
	      (debug-nd "~%~D: OI-BC: (show ~A)"
			(ftre-depth *ftre*)  ?disjunct)
       (rlet ((?disjunct ?disjunct))
	     (rassert! (show ?disjunct))
	     (rule (?disjunct) 
		   (debug-nd "~%~D: OI: ~A"
			     (ftre-depth *ftre*) (cons 'OR ?disjuncts))
		   (assert! `(or . ,?disjuncts))))))

(rule ((show ?r) ;; OR elimination
       :test (not (or (fetch ?r)
		      (eq ?r 'contradiction)
		      (not (simple-proposition? ?r))))
       (or ?d1 ?d2)
       :test  (not (or (eq ?d1 'contradiction)
		       (eq ?d2 'contradiction))))
       (debug-nd "~%~D: OE-BC: (show (implies ~A ~A))"
		 (ftre-depth *ftre*) ?d1 ?r)
       (rassert! (show (implies ?d1 ?r)))	 
       (debug-nd "~%~D: OE-BC: (show (implies ~A ~A))"
		 (ftre-depth *ftre*) ?d2 ?r)
       (rassert! (show (implies ?d2 ?r)))
       (rule ((implies ?d1 ?r) (implies ?d2 ?r))
	     (debug-nd "~% ~D: OE: ~A" (ftre-depth *ftre*) ?r)
	     (rassert! ?r)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Indirect proof and contradiction detection

(a-rule ((show ?p) ;indirect proof.
	 :test (not (or (fetch ?p)
			(eq ?p 'contradiction)
			(not (simple-proposition? ?p)))))
    (debug-nd "~%~D: IP attempt: ~A."
	      (ftre-depth *ftre*) ?p)
    (when (seek-in-context `(not ,?p)
			   'contradiction)
      (debug-nd "~%~D: IP: ~A" (ftre-depth *ftre*) ?p)
      (rassert! ?p)))

(rule ((show contradiction) ;contradiction detection
       (not ?p) ?p)
	(debug-nd "~%~D: Contra: (not ~A) and ~A"
		  (ftre-depth *ftre*) ?p ?p)
	(rassert! contradiction))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; End of Code
